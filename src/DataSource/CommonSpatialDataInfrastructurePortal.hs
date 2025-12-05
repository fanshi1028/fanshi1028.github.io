{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- NOTE: https://www.hko.gov.hk/en/abouthko/opendata_intro.htm
module DataSource.CommonSpatialDataInfrastructurePortal where

import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Data.Csv hiding (decode, encode, lookup)
import Data.Hashable
import Data.Text hiding (show)
import Data.Text.Read
import Data.Time
import Data.Time.Calendar.Julian
import Data.Typeable
import Data.Vector
import DataSource.LocalStorage
import Haxl.Core hiding (throw)
import Language.Javascript.JSaddle hiding (Object, Success)
import Network.URI
import Network.URI.Static
import Numeric.Natural
import Text.XML.Light
import Utils.Haxl
import Utils.IntervalPeriod
import Utils.Serialise

-- NOTE: CSDI Portal API
data CommonSpatialDataInfrastructurePortalReq a where
  GetLatest15minUVIndex :: IntervalPeriod 15 -> URI -> CommonSpatialDataInfrastructurePortalReq (Vector UVIndexRecord)
  GetLatest15minUVIndexGeoJSON :: IntervalPeriod 15 -> CommonSpatialDataInfrastructurePortalReq SerialisableValue

deriving instance Eq (CommonSpatialDataInfrastructurePortalReq a)

instance Hashable (CommonSpatialDataInfrastructurePortalReq a) where
  hashWithSalt s req = hashWithSalt @Int s $ case req of
    GetLatest15minUVIndex p uri' -> 0 `hashWithSalt` uriToString id uri' "" `hashWithSalt` p
    GetLatest15minUVIndexGeoJSON p -> 1 `hashWithSalt` p

deriving instance Show (CommonSpatialDataInfrastructurePortalReq a)

instance ShowP CommonSpatialDataInfrastructurePortalReq where showp = show

instance StateKey CommonSpatialDataInfrastructurePortalReq where
  newtype State CommonSpatialDataInfrastructurePortalReq = CommonSpatialDataInfrastructurePortalReqState JSContextRef

instance DataSourceName CommonSpatialDataInfrastructurePortalReq where
  dataSourceName _ = pack "CSDI Portal API"

csdiPortalReqToURI :: CommonSpatialDataInfrastructurePortalReq a -> URI
csdiPortalReqToURI (GetLatest15minUVIndex _ uri') = uri'
csdiPortalReqToURI (GetLatest15minUVIndexGeoJSON _) =
  [uri|https://portal.csdi.gov.hk/server/services/common/hko_rcd_1634894904080_80327/MapServer/WFSServer|]
    { uriQuery =
        renderQueryTextToString
          [ ("service", Just "wfs"),
            ("request", Just "GetFeature"),
            ("typenames", Just "latest_15min_uvindex"),
            ("outputFormat", Just "geojson"),
            ("maxFeatures", Just "10"),
            ("srsName", Just "EPSG:4326"),
            ( "filter",
              Just . pack . showElement $
                -- "<Filter><Intersects><PropertyName>SHAPE</PropertyName><gml:Envelope srsName='EPSG:4326'><gml:lowerCorner>22.15 113.81</gml:lowerCorner><gml:upperCorner>22.62 114.45</gml:upperCorner></gml:Envelope></Intersects></Filter>"
                unode "Filter" $
                  [ unode "Intersects" $
                      [ unode @String "PropertyName" "SHAPE",
                        unode "gml:Envelope" $
                          ( [Attr (unqual "srsName") "EPSG:4326"],
                            [ unode @String "gml:lowerCorner" "22.15 113.81",
                              unode @String "gml:upperCorner" "22.62 114.45"
                            ]
                          )
                      ]
                  ]
            )
          ]
    }

instance DataSource u CommonSpatialDataInfrastructurePortalReq where
  fetch reqState@(CommonSpatialDataInfrastructurePortalReqState jscontext) =
    backgroundFetchPar
      ( -- NOTE: sad boilerplate
        \req -> runJSaddle jscontext $ case req of
          GetLatest15minUVIndexGeoJSON _ -> fetchGetJSON Proxy $ csdiPortalReqToURI req
          GetLatest15minUVIndex _ _ -> fmap snd <$> fetchGetCSVNamed Proxy (corsProxy $ csdiPortalReqToURI req)
      )
      reqState

getLatest15minUVIndexGeoJSON :: UTCTime -> GenHaxl u w SerialisableValue
getLatest15minUVIndexGeoJSON = fetchCacheable . GetLatest15minUVIndexGeoJSON . utcTimeToIntervalPeriod Proxy

getLatest15minUVIndex :: UTCTime -> URI -> GenHaxl u w (Vector UVIndexRecord)
getLatest15minUVIndex t uri' = fetchCacheable $ GetLatest15minUVIndex (utcTimeToIntervalPeriod Proxy t) uri'

data UVIndexRecord = UVIndexRecord UTCTime Natural deriving (Show, Eq)

instance FromNamedRecord UVIndexRecord where
  parseNamedRecord m = do
    fromJulianValid <$> m .: "Date time (Year)" <*> m .: "Date time (Month)" <*> m .: "Date time (Day)" >>= \case
      Nothing -> fail "Invalid Day"
      Just day -> do
        timeOfDay <-
          TimeOfDay
            <$> m .: "Date time (Hour)"
            <*> m .: "Date time (Minute)"
            -- sec <- m .: "Date time (Second)"
            <*> pure 0
        tz <-
          stripPrefix "UTC+" <$> m .: "Date time (Time Zone)" >>= \case
            Nothing -> fail "expected prefix: 'UTC+' for 'Date time (Time Zone)'"
            Just i -> case decimal i of
              Left err -> fail err
              Right (i', "") -> pure $ hoursToTimeZone i'
              Right (_, leftover) -> fail $ "unexpected suffix for " <> unpack leftover <> " 'Date time (Time Zone)'"
        idx <- m .: "past 15-minute mean UV Index"
        pure $ UVIndexRecord (zonedTimeToUTC $ ZonedTime (LocalTime day timeOfDay) tz) idx

instance Serialise UVIndexRecord where
  encode (UVIndexRecord t idx) = encodeListLen 3 <> encodeWord 0 <> encode t <> encode idx
  decode =
    (,) <$> decodeListLen <*> decodeWord >>= \case
      (3, 0) -> UVIndexRecord <$> decode <*> decode
      _ -> fail "invalid UVIndexRecord encoding"
