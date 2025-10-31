{-# LANGUAGE CPP #-}

module MapLibre (mapLibreComponent, createMap, runMapLibre, mapLibreEaseTo, mapLibreAddMarker) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Void
import Language.Javascript.JSaddle hiding (new)
import Miso hiding ((<#))
import Miso.Html.Element
import Miso.Html.Property
import Miso.Navigator
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Async

#ifndef javascript_HOST_ARCH
import Language.Javascript.JSaddle.Object qualified as JS (new)
#endif

new :: (MakeObject constructor, MakeArgs args) => constructor -> args -> JSM JSVal
#ifndef javascript_HOST_ARCH
new = JS.new
#endif
#ifdef javascript_HOST_ARCH
foreign import javascript unsafe "(constructor, args) => new constructor(...args)"
    jsNew :: Object -> [JSVal] -> IO JSVal 
new constructor args = do
     arg' <- makeArgs args  
     constructor' <- makeObject constructor  
     liftIO $ jsNew constructor' arg'
#endif

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
      div_ [] [div_ [id_ mapLibreId, class_ $ ms "self-stretch h-72"] []]
  )
    { scripts = [Src $ toJSString "https://unpkg.com/maplibre-gl@latest/dist/maplibre-gl.js"],
      styles = [Href $ toJSString "https://unpkg.com/maplibre-gl@latest/dist/maplibre-gl.css"]
    }

mapLibreAddMarker :: Geolocation -> JSM ()
mapLibreAddMarker (Geolocation lat lon acc) = do
  marker <-
    liftIO (readMVar mapLibreLibMVar) >>= \mapLibreLib -> Marker <$> new (mapLibreLib ! "Marker") ()
  mapLibre <- liftIO $ readMVar mapLibreMVar
#ifndef javascript_HOST_ARCH
  void $ marker # "setLngLat" $ [[lon, lat]]
  void $ marker # "addTo" $ [mapLibre]
#endif
-- TEMP fix for jsaddle FIXME
#ifdef javascript_HOST_ARCH
  liftIO $ jsMapLibreAddMarker mapLibre marker lon lat

foreign import javascript unsafe "(map, marker, lon, lat) => marker.setLngLat([lon, lat]).addTo(map)"
  jsMapLibreAddMarker :: MapLibre -> Marker -> Double -> Double -> IO ()
#endif

mapLibreEaseTo :: Geolocation -> JSM ()
mapLibreEaseTo (Geolocation lat lon acc) = do
  mapLibre <- liftIO $ readMVar mapLibreMVar
  cfg <- obj
  cfg <# "around" $ [lon, lat]
  cfg <# "center" $ [lon, lat]
  cfg <# "zoom" $ 5
#ifndef javascript_HOST_ARCH
  void $ mapLibre # "easeTo" $ [cfg]
#endif
-- TEMP fix for jsaddle FIXME
#ifdef javascript_HOST_ARCH
  liftIO $ jsMapLibreEaseTo mapLibre cfg

foreign import javascript unsafe "(map, cfg) => map.easeTo(cfg)"
  jsMapLibreEaseTo :: MapLibre -> Object ->  IO ()
#endif


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
    cfg <# "style" $ "https://demotiles.maplibre.org/style.json"
    mapLibre <- new (maplibregl ! "Map") [cfg]
    jsg "miso" <# "mapLibre" $ mapLibre
    liftIO . putMVar mapLibreMVar $ MapLibre mapLibre
