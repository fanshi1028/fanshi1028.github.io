{-# LANGUAGE CPP #-}

module MapLibre (mapLibreComponent, createMap, cleanUpMap, runMapLibre, addMarkerAndEaseToLocation) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Void
import Language.Javascript.JSaddle
import Miso hiding ((<#))
import Miso.Html.Element
import Miso.Html.Property
import Miso.Navigator
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Async

mapLibreId :: MisoString
mapLibreId = ms "mapLibreId-14yMVNtDA3GBoGwMHBcDu5bhKUHu/9gcFx41dNF+2Zg="

newtype MapLibreLib = MapLibreLib JSVal deriving newtype (ToJSVal, MakeObject)

mapLibreLibMVar :: MVar MapLibreLib
mapLibreLibMVar = unsafePerformIO newEmptyMVar

newtype MapLibre = MapLibre JSVal deriving newtype (ToJSVal, MakeObject)

mapLibreMVar :: MVar MapLibre
mapLibreMVar = unsafePerformIO newEmptyMVar

newtype Marker = Marker JSVal deriving newtype (ToJSVal, MakeObject)

mapLibreComponent :: Component parent () Void
mapLibreComponent =
  ( component () absurd $ \_ ->
      div_ [id_ $ ms "FIXME: need to wrap the div with id ${mapLibreId} to TEMP fix the 'conatianer not found' for maplibre", class_ $ ms "h-full"] [div_ [id_ mapLibreId, class_ $ ms "h-full"] []]
  )
    { scripts,
      styles = [Href $ toJSString "https://unpkg.com/maplibre-gl@latest/dist/maplibre-gl.css"]
    }
  where
#ifndef javascript_HOST_ARCH
    scripts = [Src $ toJSString "./typescript/maplibre-gl-ffi/index.js"]
#endif
#ifdef javascript_HOST_ARCH
    scripts = []
#endif

addMarkerAndEaseToLocation :: Geolocation -> JSM ()
addMarkerAndEaseToLocation (Geolocation lat lon acc) = do
  mapLibre <- liftIO $ readMVar mapLibreMVar
#ifndef javascript_HOST_ARCH
  mapLibreLib <- liftIO (readMVar mapLibreLibMVar)
  void $ (mapLibreLib # "addMarkerAndEaseToLocation") (lon, lat, mapLibre)
#endif
#ifdef javascript_HOST_ARCH
  toJSVal mapLibre >>= liftIO . addMarkerAndEaseToLocationJs lon lat

foreign import javascript unsafe "addMarkerAndEaseToLocation"
  addMarkerAndEaseToLocationJs :: Double -> Double -> JSVal -> IO ()
#endif

runMapLibre :: ReaderT MapLibreLib JSM a -> JSM a
runMapLibre m = do
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
  map' <- makeMap
  liftIO . putMVar mapLibreMVar $ MapLibre map'
  where
#ifndef javascript_HOST_ARCH
    makeMap = do
      maplibregl <- ask
      liftJSM $ maplibregl # "createMap" $ mapLibreId
#endif
#ifdef javascript_HOST_ARCH
    makeMap = liftJSM (toJSVal mapLibreId) >>= liftIO . createMapJS

foreign import javascript unsafe "createMap"
  createMapJS :: JSVal -> IO ()
#endif

cleanUpMap :: JSM ()
cleanUpMap = () <$ liftIO (takeMVar mapLibreMVar)
