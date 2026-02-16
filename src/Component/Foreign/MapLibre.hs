{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Component.Foreign.MapLibre
  ( mapLibreComponent,
    createMap,
    cleanUpMap,
    runMapLibre,
    addMarkerAndEaseToLocation,
    addGeoJSONSource,
    toggle_hssp7,
  )
where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Functor
import Data.Void
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
import Prelude hiding (show, (!!), (+))

#ifdef LOCALDEV
import Embed.Dev
#endif

mapLibreId :: MisoString
mapLibreId = "mapLibreId-14yMVNtDA3GBoGwMHBcDu5bhKUHu/9gcFx41dNF+2Zg="

newtype MapLibreLib = MapLibreLib JSVal deriving newtype (ToJSVal, ToObject)

newtype MapLibre = MapLibre JSVal deriving newtype (ToJSVal, ToObject)

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

geolocationToLngLat :: Geolocation -> LngLat
geolocationToLngLat (Geolocation lat lon _acc) = LngLat (lon *~ degree) (lat *~ degree)

addMarkerAndEaseToLocation :: Geolocation -> ReaderT MapLibreLib IO ()
addMarkerAndEaseToLocation (geolocationToLngLat -> loc) = do
  mapLibreLib <- ask
  mapLibre <- liftIO $ readMVar mapLibreMVar
  void . liftIO $ mapLibreLib # "addMarkerAndEaseToLocation" $ (loc, mapLibre)

runMapLibre :: ReaderT MapLibreLib IO a -> IO a
runMapLibre m = do
  mapLibreLibMVar <- newEmptyMVar
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

createMap :: ReaderT MapLibreLib IO ()
createMap = do
  mapLibreLib <- ask
  void . liftIO $ do
    map' <- mapLibreLib # "createMap" $ mapLibreId
    withAsync
      ( forever $ do
          loaded <- (map' # "loaded") ()
          fromJSVal loaded >>= \case
            Nothing -> do
              unexpected <- jsonStringify loaded
              consoleError $ "Unexpected(createMap): expected loaded to be Bool but got " <> unexpected
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

addGeoJSONSource :: (ToJSVal a) => MisoString -> a -> ReaderT MapLibreLib IO ()
addGeoJSONSource sourceId v = do
  mapLibreLib <- ask
  void . liftIO $ do
    mapLibre <- readMVar mapLibreMVar
    mapLibreLib # "renderUVIndexGeoJSON" $ (mapLibre, sourceId, v)

data LngLat = LngLat (Quantity DPlaneAngle Double) (Quantity DPlaneAngle Double)
  deriving stock (Show)

instance ToJSVal LngLat where
  toJSVal (LngLat lng lat) = toJSVal (lng /~ degree, lat /~ degree)

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

toggle_hssp7 :: ReaderT MapLibreLib IO ()
toggle_hssp7 = do
  mapLibreLib <- ask
  void . liftIO $ do
    hssp7Lib <- mapLibreLib ! "hard_surface_soccer_pitch_7"
    mapLibre <- readMVar mapLibreMVar
    (hssp7Lib # "getFeatures") () >>= fromJSVal @(Maybe JSVal) >>= \case
      Nothing -> consoleError "impossible: hard_surface_soccer_pitch_7 getFeatures return unexpected result"
      Just Nothing -> do
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
            Nothing -> consoleError "impossible: hard_surface_soccer_pitch_7 data is not an array"
        void $ hssp7Lib # "toggleLayer" $ (mapLibre, processCoords)
      Just _ -> void $ hssp7Lib # "toggleLayer" $ [mapLibre]
