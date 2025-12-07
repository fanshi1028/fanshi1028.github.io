{-# LANGUAGE CPP #-}

module Component.Foreign.MapLibre
  ( mapLibreComponent,
    createMap,
    cleanUpMap,
    runMapLibre,
    addMarkerAndEaseToLocation,
    getUVIndexDataURI,
    addGeoJSONSource,
    render_hssp7,
  )
where

import Control.Concurrent
import Control.Exception (SomeException, toException)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Functor
import Data.List
import Data.Text hiding (length)
import Data.Void
import Haxl.Core.Exception
import Language.Javascript.JSaddle
import Miso hiding (URI, get, (<#))
import Miso.Html.Element
import Miso.Html.Property
import Miso.Navigator
import Network.URI
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.Quantities
import Numeric.Units.Dimensional.SIUnits
import System.IO.Unsafe (unsafePerformIO)
import Text.Read
import UnliftIO.Async
import Prelude hiding (show, (!!), (+))

mapLibreId :: MisoString
mapLibreId = ms "mapLibreId-14yMVNtDA3GBoGwMHBcDu5bhKUHu/9gcFx41dNF+2Zg="

newtype MapLibreLib = MapLibreLib JSVal deriving newtype (ToJSVal, MakeObject)

newtype MapLibre = MapLibre JSVal deriving newtype (ToJSVal, MakeObject)

mapLibreMVar :: MVar MapLibre
mapLibreMVar = unsafePerformIO newEmptyMVar

mapLibreComponent :: Component parent () Void
mapLibreComponent =
  ( component () absurd $ \_ ->
      div_ [id_ $ ms "FIXME: need to wrap the div with id ${mapLibreId} to TEMP fix the 'conatianer not found' for maplibre", class_ $ ms "h-full"] [div_ [id_ mapLibreId, class_ $ ms "h-full"] []]
  )
    { scripts,
      styles
    }
  where
#ifndef PRODUCTION
    scripts = [Src $ toJSString "typescript/maplibre-gl-ffi/index.js"]
    styles = [Href $ toJSString "typescript/maplibre-gl-ffi/node_modules/maplibre-gl/dist/maplibre-gl.css"]
#endif
#ifdef PRODUCTION
#ifdef WASM
    scripts = [Src $ toJSString "../maplibre-gl-ffi.js"]
    styles = [Href $ toJSString "../maplibre-gl.css"]
#endif
#ifndef WASM
    scripts = [Src $ toJSString "maplibre-gl-ffi.js"]
    styles = [Href $ toJSString "maplibre-gl.css"]
#endif
#endif

addMarkerAndEaseToLocation :: Geolocation -> ReaderT MapLibreLib JSM ()
addMarkerAndEaseToLocation (Geolocation lat lon acc) = do
  mapLibreLib <- ask
  mapLibre <- liftIO $ readMVar mapLibreMVar
  void . liftJSM $ (mapLibreLib # "addMarkerAndEaseToLocation") (lon, lat, mapLibre)

runMapLibre :: ReaderT MapLibreLib JSM a -> JSM a
runMapLibre m = do
  mapLibreLibMVar <- liftIO newEmptyMVar
  mapLibreLib <-
    liftIO (tryReadMVar mapLibreLibMVar) >>= \case
      Just r -> pure r
      Nothing -> withAsync
        ( forever $ do
            maplibregl <- jsg "maplibregl_ffi"
            maplibreglDefined <- not <$> valIsUndefined maplibregl
            when (maplibreglDefined) $ liftIO $ putMVar mapLibreLibMVar $ MapLibreLib maplibregl
            liftIO $ threadDelay 100000
        )
        $ \_ -> liftIO (readMVar mapLibreLibMVar)
  runReaderT m mapLibreLib

createMap :: ReaderT MapLibreLib JSM ()
createMap = do
  mapLibreLib <- ask
  void . liftJSM $ do
    map' <- mapLibreLib # "createMap" $ mapLibreId
    withAsync
      ( forever $ do
          loaded <- (map' # "loaded") () >>= fromJSValUnchecked
          when loaded $ liftIO . putMVar mapLibreMVar $ MapLibre map'
          liftIO $ threadDelay 100000
      )
      $ \_ -> liftIO (readMVar mapLibreMVar)

cleanUpMap :: JSM ()
cleanUpMap = do
  liftIO (tryTakeMVar mapLibreMVar) >>= \case
    Just mapLibre -> void $ mapLibre # "remove" $ ()
    Nothing -> consoleWarn $ ms "cleanUpMap: no map to be cleaned up"

getUVIndexDataURI :: (ToJSVal geoJSON) => geoJSON -> ReaderT MapLibreLib JSM (Either SomeException URI)
getUVIndexDataURI geoJSON = do
  mapLibreLib <- ask
  dataURIMVar <- liftIO $ newEmptyMVar
  liftJSM $ do
    callback <- function $ \_ _ -> \case
      [url] ->
        fromJSValUnchecked url <&> parseAbsoluteURI >>= \case
          Nothing -> do
            invalidURI <-
              fromJSVal url >>= \case
                Just url' -> pure url'
                Nothing -> (jsg "JSON" # "stringify" $ [url]) >>= fromJSValUnchecked
            liftIO . putMVar dataURIMVar . Left $
              toException . InvalidParameter $
                pack "impossible: getDataURI callback expect valid uri but got " <> invalidURI
          Just data_uri -> liftIO . putMVar dataURIMVar $ Right data_uri
      args ->
        liftIO . putMVar dataURIMVar . Left $
          toException . InvalidParameter $
            pack "impossible: getDataURI callback expect 1 args but got " <> show (length args)
    _ <- mapLibreLib # "getDataURI" $ (geoJSON, callback)
    r <- liftIO $ takeMVar dataURIMVar
    freeFunction callback
    pure r

addGeoJSONSource :: (ToJSVal a) => MisoString -> a -> ReaderT MapLibreLib JSM ()
addGeoJSONSource sourceId v = do
  mapLibreLib <- ask
  mapLibre <- liftIO $ readMVar mapLibreMVar
  void . liftJSM $ mapLibreLib # "renderUVIndexGeoJSON" $ (mapLibre, sourceId, v)

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

render_hssp7 :: ReaderT MapLibreLib JSM ()
render_hssp7 = do
  mapLibreLib <- ask
  void . liftJSM $ do
    coords <-
      mapLibreLib ! "hssp7"
        >>= fromJSVal @[JSVal]
        >>= \case
          Just hssp7s ->
            traverse
              ( \hssp7 -> do
                  mLng <- hssp7 ! "Longitude" >>= fromJSVal <&> (>>= fromWGS84Str)
                  mLat <- hssp7 ! "Latitude" >>= fromJSVal <&> (>>= fromWGS84Str)
                  pure $ LngLat <$> mLng <*> mLat
              )
              hssp7s
          Nothing -> do
            _ <- jsg "console" # "error" $ "impossible; hssp7 is not an array"
            pure []
    mapLibre <- liftIO $ readMVar mapLibreMVar
    void . liftJSM $ mapLibreLib # "render_hssp7" $ (mapLibre, coords)
