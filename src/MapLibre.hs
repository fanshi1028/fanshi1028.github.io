module MapLibre where

import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Foldable
import Language.Javascript.JSaddle.Object
import Language.Javascript.JSaddle.Types
import Language.Javascript.JSaddle.Value
import Miso (consoleLog, toMisoString)
import Miso.Navigator hiding (new)

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
  maplibregl <- mfix $ \maplibregl -> do
    jsg "maplibregl" >>= \maplibregl' ->
      valIsUndefined maplibregl' >>= \case
        False -> pure maplibregl'
        True -> do
          liftIO (threadDelay 150000)
          consoleLog $ toMisoString "hi"
          pure maplibregl
  new (maplibregl ! "Map") [mkMapLibreCfg id' geo]
