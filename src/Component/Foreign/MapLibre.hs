{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Component.Foreign.MapLibre
  ( mapLibreComponent,
    callMapLibreFunction,
    callMapLibreFunctionWithMap,
    createMap,
    cleanUpMap,
    runMapLibre,
    addMarkerAndEaseToLocation,
    focusDistrict,
    toggle_hssp7,
    LngLat (..),
    geolocationToLngLat,
  )
where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Functor
import Data.Text
import Miso hiding (URI, get, (<#))
import Miso.Html.Element
import Miso.Html.Property
import Miso.JSON
import Miso.Navigator
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.Quantities
import Numeric.Units.Dimensional.SIUnits
import System.IO.Unsafe (unsafePerformIO)
import Text.Read
import UnliftIO.Async
import Utils.JS
import Prelude hiding (show, (!!), (+))

#ifdef LOCALDEV
import Embed.Dev
#endif

mapLibreId :: MisoString
mapLibreId = "mapLibreId-14yMVNtDA3GBoGwMHBcDu5bhKUHu/9gcFx41dNF+2Zg="

newtype MapLibreLib = MapLibreLib JSVal deriving newtype (ToJSVal, ToObject)

newtype MapLibre = MapLibre JSVal deriving newtype (ToJSVal, ToObject)

mapLibreLibMVar :: MVar MapLibreLib
mapLibreLibMVar = unsafePerformIO newEmptyMVar

mapLibreMVar :: MVar MapLibre
mapLibreMVar = unsafePerformIO newEmptyMVar

data Action = CleanUpMapLibre

mapLibreComponent :: Component parent () Action
mapLibreComponent =
  ( component () (\CleanUpMapLibre -> io_ $ cleanUpMap) $
      \() -> div_ [id_ mapLibreId, class_ "h-full"] []
  )
    { scripts,
      styles,
      unmount = Just CleanUpMapLibre
    }
  where
#ifdef LOCALDEV
    scripts = [Script $ ms maplibreglJS]
    styles = [Style $ ms maplibreglCSS]
#endif
#ifdef PRODUCTION
#ifdef WASM
    scripts = [Src "../maplibre-gl-ffi.js"]
    styles = [Href "../maplibre-gl.css"]
#endif
#ifndef WASM
    scripts = [Src "maplibre-gl-ffi.js"]
    styles = [Href "maplibre-gl.css"]
#endif
#endif

runMapLibre :: ReaderT MapLibreLib IO a -> IO a
runMapLibre m = do
  mapLibreLib <-
    tryReadMVar mapLibreLibMVar >>= \case
      Just r -> pure r
      Nothing -> withAsync
        ( forever $ do
            maplibregl <- jsg "maplibregl_ffi"
            maplibreglDefined <- not <$> isUndefined maplibregl
            when (maplibreglDefined) $ putMVar mapLibreLibMVar $ MapLibreLib maplibregl
            threadDelay 100000
        )
        $ \_ -> readMVar mapLibreLibMVar
  runReaderT m mapLibreLib

callMapLibreFunction :: (ToArgs args) => MisoString -> args -> IO JSVal
callMapLibreFunction fun args = runMapLibre $ do
  mapLibreLib <- ask
  liftIO (mapLibreLib # fun $ args)

callMapLibreFunctionWithMap :: (ToJSVal arg) => MisoString -> arg -> IO JSVal
callMapLibreFunctionWithMap fun arg = runMapLibre $ do
  mapLibreLib <- ask
  liftIO $ do
    mapLibre <- readMVar mapLibreMVar
    mapLibreLib # fun $ (mapLibre, arg)

createMap :: IO MapLibre
createMap = do
  map' <- callMapLibreFunction "createMap" mapLibreId
  withAsync
    ( forever $ do
        loaded <- (map' # "loaded") ()
        fromJSVal loaded >>= \case
          Nothing -> consoleError' ("Unexpected(createMap): expected loaded to be Bool but got %o" :: MisoString, loaded)
          Just loaded' -> do
            when loaded' $ putMVar mapLibreMVar $ MapLibre map'
            threadDelay 100000
    )
    $ \_ -> readMVar mapLibreMVar

cleanUpMap :: IO ()
cleanUpMap =
  tryTakeMVar mapLibreMVar >>= \case
    Just mapLibre -> void $ mapLibre # "remove" $ ()
    Nothing -> consoleWarn "cleanUpMap: no map to be cleaned up"

geolocationToLngLat :: (Fractional a) => Geolocation -> LngLat a
geolocationToLngLat (Geolocation lat lon _) = realToFrac <$> LngLat (lon *~ degree) (lat *~ degree)

addMarkerAndEaseToLocation :: Geolocation -> IO ()
addMarkerAndEaseToLocation =
  void
    . callMapLibreFunctionWithMap "addMarkerAndEaseToLocation"
    . geolocationToLngLat @Double

focusDistrict :: StrictText -> IO ()
focusDistrict = void . callMapLibreFunctionWithMap "focusDistrict"

data LngLat a = LngLat (Quantity DPlaneAngle a) (Quantity DPlaneAngle a)
  deriving stock (Eq, Show, Functor)

instance (ToJSVal a, Real a) => ToJSVal (LngLat a) where
  toJSVal (fmap (realToFrac @_ @Double) -> LngLat lng lat) = toJSVal (lng /~ degree, lat /~ degree)

fromWGS84Str :: String -> Maybe (Quantity DPlaneAngle Double)
fromWGS84Str str =
  let readFromWGS84Str = do
        deg <- (*~ degree) . fromIntegral @_ @Double <$> readPrec @Int
        '-' <- get
        min' <- (*~ arcminute) . fromIntegral <$> readPrec @Int
        '-' <- get
        sec <- (*~ arcsecond) . fromIntegral <$> readPrec @Int
        pure $ deg + min' + sec
   in case [x | (x, "") <- readPrec_to_S readFromWGS84Str minPrec str] of
        [x] -> Just x
        _ -> Nothing

toggle_hssp7 :: IO ()
toggle_hssp7 = runMapLibre $ do
  mapLibreLib <- ask
  void . liftIO $ do
    hssp7Lib <- mapLibreLib ! "hard_surface_soccer_pitch_7"
    mapLibre <- readMVar mapLibreMVar
    (hssp7Lib # "getFeatures") () >>= isNull >>= \case
      True -> do
        processCoords <- syncCallback2 $ \hssp7_data setCoords ->
          fromJSVal @[JSVal] hssp7_data >>= \case
            Just hssp7s ->
              void . call setCoords global $
                [ traverse
                    ( \hssp7 -> do
                        mLng <- hssp7 ! "Longitude" >>= fromJSVal <&> (>>= fromWGS84Str)
                        mLat <- hssp7 ! "Latitude" >>= fromJSVal <&> (>>= fromWGS84Str)
                        pure $ LngLat <$> mLng <*> mLat
                    )
                    hssp7s
                ]
            Nothing -> consoleError' ("impossible! hard_surface_soccer_pitch_7 data is not an array: %o" :: MisoString, hssp7_data)
        void $ hssp7Lib # "toggleLayer" $ (mapLibre, processCoords)
      False -> void $ hssp7Lib # "toggleLayer" $ [mapLibre]
