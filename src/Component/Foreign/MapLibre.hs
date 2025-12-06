{-# LANGUAGE CPP #-}

module Component.Foreign.MapLibre
  ( mapLibreComponent,
    createMap,
    cleanUpMap,
    runMapLibre,
    addMarkerAndEaseToLocation,
    getUVIndexDataURI,
    addGeoJSONSource,
    getHardSurfaceSoccerPitches7aSideInfo,
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

-- NOTE: https://www.lcsd.gov.hk/datagovhk/facility/facility-hssp7_data_dictionary.pdf
{- example item <2025-12-01 Mon>:
{
    "Address_cn": "九龍何文田忠義街一號",
    "Address_en": "No.1 Chung Yee Street, Ho Man Tin, Kowloon.",
    "Ancillary_facilities_cn": "<ul>\r\n<li>男、女更衣室及洗手間</li>\r\n<li>1個收費停車場 (設有1個殘疾人士專用車位)</li>\r\n<li>其他設施包括2個籃球場、1個兒童遊樂場、1條緩跑徑及6個健身站</li>\r\n<li>此球場可進行7人足球或手球活動</li>\r\n<li>設有可容納480人的看台</li>\r\n<li>無障礙設施：暢通易達洗手間、觸覺引路帶、觸覺點字及觸覺平面圖</li>\r\n</ul>",
    "Ancillary_facilities_en": "<ul>\r\n<li>Men's and ladies' changing rooms and toilets</li>\r\n<li>A fee-charging car park (including 1 designated disabled parking space)</li>\r\n<li>Other facilities include 2 basketball courts, a children's playground and a jogging track with 6 fitness stations.</li>\r\n<li>This court can be used for playing 7-a-side soccer or handball.</li>\r\n<li>A spectator stand with 480 seats is provided.</li>\r\n<li>Barrier Free Facilities: Accessible Toilet, Tactile Guide Path, Braille Directory Map/Floor Plan</li>\r\n</ul>",
    "Court_no_cn": "1",
    "Court_no_en": "1",
    "District_cn": "九龍城區",
    "District_en": "Kowloon City",
    "GIHS": "MSKgwBPmtd",
    "Latitude": "22-18-44",
    "Longitude": "114-10-50",
    "Name_cn": "何文田公園",
    "Name_en": "Ho Man Tin Park",
    "Opening_hours_cn": "每日上午7時至晚上11時",
    "Opening_hours_en": "7 am to 11 pm daily",
    "Phone": "2762 7837",
    "Remarks_cn": "",
    "Remarks_en": ""
}
-}
data HardSurfaceSoccerPitches7aSideInfo = HardSurfaceSoccerPitches7aSideInfo
  { latitude :: Quantity DPlaneAngle Double,
    longitude :: Quantity DPlaneAngle Double
  }
  deriving stock (Show)

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

instance FromJSVal HardSurfaceSoccerPitches7aSideInfo where
  fromJSVal v = do
    mLat <- v ! "Latitude" >>= fromJSVal <&> (>>= fromWGS84Str)
    mLng <- v ! "Longitude" >>= fromJSVal <&> (>>= fromWGS84Str)
    pure $ HardSurfaceSoccerPitches7aSideInfo <$> mLat <*> mLng

getHardSurfaceSoccerPitches7aSideInfo :: ReaderT MapLibreLib JSM [HardSurfaceSoccerPitches7aSideInfo]
getHardSurfaceSoccerPitches7aSideInfo = do
  mapLibreLib <- ask
  liftJSM $ mapLibreLib ! "hssp7" >>= fromJSValUnchecked

render_hssp7 :: ReaderT MapLibreLib JSM ()
render_hssp7 = do
  mapLibreLib <- ask
  hssp7 <- getHardSurfaceSoccerPitches7aSideInfo
  let hssp7' = [(lat /~ degree, lng /~ degree) | HardSurfaceSoccerPitches7aSideInfo lat lng <- hssp7]
  void . liftJSM $ do
    mapLibre <- liftIO $ readMVar mapLibreMVar
    void . liftJSM $ mapLibreLib # "render_hssp7" $ (mapLibre, hssp7')
