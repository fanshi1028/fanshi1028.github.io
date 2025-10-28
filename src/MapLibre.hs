module MapLibre where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Language.Javascript.JSaddle.Object
import Language.Javascript.JSaddle.Types
import Language.Javascript.JSaddle.Value
import Miso (consoleLog, toMisoString)
import Miso.Navigator hiding (new)
import UnliftIO.Async
import UnliftIO.Exception

mkMapLibreCfg :: JSString -> Geolocation -> JSM Object
mkMapLibreCfg id' (Geolocation lat lon acc) = do
  cfg <- obj
  (cfg <# "container") id'
  (cfg <# "style") "https://demotiles.maplibre.org/style.json"
  (cfg <# "center") [lat, lon]
  (cfg <# "zoom") 5
  pure cfg

createMapLibre :: JSString -> Geolocation -> JSM JSVal
createMapLibre id' geo = do
  mapLibreMVar <- liftIO newEmptyMVar
  withAsync
    ( forever $ do
        maplibregl <- jsg "maplibregl"
        valIsUndefined maplibregl >>= \case
          False -> new (maplibregl ! "Map") [mkMapLibreCfg id' geo] >>= liftIO . putMVar mapLibreMVar
          True -> consoleLog $ toMisoString "hi"
        liftIO (threadDelay 100000)
    )
    $ \ensureMaplibregl -> liftIO (readMVar mapLibreMVar) `finally` uninterruptibleCancel ensureMaplibregl
