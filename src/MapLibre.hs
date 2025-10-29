module MapLibre where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Language.Javascript.JSaddle.Object
import Language.Javascript.JSaddle.Types
import Language.Javascript.JSaddle.Value
import Miso.Navigator
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Async

mapLibreMVar :: MVar JSVal
mapLibreMVar = unsafePerformIO newEmptyMVar

mkMapLibreCfg :: JSString -> Geolocation -> JSM Object
mkMapLibreCfg id' (Geolocation lat lon acc) = do
  cfg <- obj
  (cfg <# "container") id'
  (cfg <# "style") "https://demotiles.maplibre.org/style.json"
  (cfg <# "center") [lat, lon]
  (cfg <# "zoom") 5
  pure cfg

createMapLibre :: JSString -> Geolocation -> JSM ()
createMapLibre id' geo =
  liftIO (tryReadMVar mapLibreMVar) >>= \case
    Just _ -> pure ()
    Nothing -> withAsync
      ( forever $ do
          maplibregl <- jsg "maplibregl"
          maplibreglDefined <- not <$> valIsUndefined maplibregl
          when (maplibreglDefined) $
            new (maplibregl ! "Map") [mkMapLibreCfg id' geo]
              >>= liftIO . putMVar mapLibreMVar
          liftIO $ threadDelay 100000
      )
      $ \_ -> () <$ liftIO (readMVar mapLibreMVar)
