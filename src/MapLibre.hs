{-# LANGUAGE CPP #-}

module MapLibre (mapLibreComponent, createMap, cleanUpMap, runMapLibre, mapLibreEaseTo, mapLibreAddMarker) where

import Control.Concurrent
import Control.Concurrent.Async
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
    { scripts = [Src $ toJSString "https://unpkg.com/maplibre-gl@latest/dist/maplibre-gl.js"],
      styles = [Href $ toJSString "https://unpkg.com/maplibre-gl@latest/dist/maplibre-gl.css"]
    }

mapLibreAddMarker :: Geolocation -> JSM ()
mapLibreAddMarker (Geolocation lat lon acc) = do
  mapLibre <- liftIO $ readMVar mapLibreMVar
  mapLibreLib <- liftIO (readMVar mapLibreLibMVar)
  marker <- Marker <$> new (mapLibreLib ! "Marker") ()
  void $ marker # "setLngLat" $ [[lon, lat]]
  void $ marker # "addTo" $ [mapLibre]

mapLibreEaseTo :: Geolocation -> JSM ()
mapLibreEaseTo (Geolocation lat lon acc) = void $ do
  mapLibre <- liftIO $ readMVar mapLibreMVar
  cfg <- obj
  cfg <# "center" $ [lon, lat]
  void $ mapLibre # "easeTo" $ [cfg]

-- mapLibre # "cameraForBounds" $ [[lon - acc, lat - acc, lon + acc, lat + acc]]

runMapLibre :: ReaderT MapLibreLib JSM a -> JSM a
runMapLibre m = do
  mapLibreLib <-
    liftIO (tryReadMVar mapLibreLibMVar) >>= \case
      Just r -> pure r
      Nothing -> withAsync
        ( forever $ do
            maplibregl <- jsg "maplibregl"
            maplibreglDefined <- not <$> valIsUndefined maplibregl
            when (maplibreglDefined) $ liftIO $ putMVar mapLibreLibMVar $ MapLibreLib maplibregl
            liftIO $ threadDelay 100000
        )
        $ \_ -> liftIO (readMVar mapLibreLibMVar)
  runReaderT m mapLibreLib

createMap :: ReaderT MapLibreLib JSM ()
createMap = do
  maplibregl <- ask
  liftJSM $ do
    cfg <- obj
    cfg <# "container" $ mapLibreId
    cfg <# "style" $ "https://tiles.openfreemap.org/styles/liberty"
    cfg <# "zoom" $ 12
    new (maplibregl ! "Map") [cfg]
      >>= liftIO . putMVar mapLibreMVar . MapLibre

cleanUpMap :: JSM ()
cleanUpMap = () <$ liftIO (takeMVar mapLibreMVar)
