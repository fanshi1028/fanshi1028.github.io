module MapLibre where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
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

newtype LibreMap = LibreMap JSVal deriving newtype (ToJSVal)

mapLibreMVar :: MVar LibreMap
mapLibreMVar = unsafePerformIO newEmptyMVar

libreMapComponent :: Component parent () Void
libreMapComponent =
  ( component () absurd $ \() ->
      div_ [] [div_ [id_ mapLibreId, class_ $ ms "self-stretch h-72"] []]
  )
    { scripts = [Src $ toJSString "https://unpkg.com/maplibre-gl@latest/dist/maplibre-gl.js"],
      styles = [Href $ toJSString "https://unpkg.com/maplibre-gl@latest/dist/maplibre-gl.css"]
    }

libreMapEaseTo :: LibreMap -> Geolocation -> JSM ()
libreMapEaseTo libreMap (Geolocation lat lon acc) = void $ do
  cfg <- obj
  cfg <# "around" $ [lat, lon]
  val libreMap # "easeTo" $ [cfg]

createMapLibre :: JSM ()
createMapLibre = do
  liftIO (tryReadMVar mapLibreMVar) >>= \case
    Just _ -> pure ()
    Nothing -> withAsync
      ( forever $ do
          maplibregl <- jsg "maplibregl"
          maplibreglDefined <- not <$> valIsUndefined maplibregl

          when (maplibreglDefined) $ do
            cfg <- obj
            cfg <# "container" $ mapLibreId
            cfg <# "style" $ "https://demotiles.maplibre.org/style.json"
            cfg <# "zoom" $ 2
            new (maplibregl ! "Map") [cfg]
              >>= liftIO . putMVar mapLibreMVar . LibreMap
          liftIO $ threadDelay 100000
      )
      $ \_ -> () <$ liftIO (readMVar mapLibreMVar)
