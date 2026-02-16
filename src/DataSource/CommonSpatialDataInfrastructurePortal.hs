{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- NOTE: https://www.hko.gov.hk/en/abouthko/opendata_intro.htm
module DataSource.CommonSpatialDataInfrastructurePortal where

import Data.Csv hiding (decode, encode, lookup, (.:))
import Data.Csv qualified as CSV ((.:))
import Data.Hashable
import Data.Text hiding (show)
import Data.Text.Read
import Data.Time
import Data.Time.Calendar.Julian
import Data.Typeable
import DataSource.LocalStorage
import GHC.Generics
import Haxl.Core hiding (throw)
import Miso.DSL
import Miso.JSON
import Network.URI
import Network.URI.Static
import Text.XML.Light
import Utils.Haxl
import Utils.IntervalPeriod
import Utils.JSON

-- NOTE: CSDI Portal API
data CommonSpatialDataInfrastructurePortalReq a where
  GetLatest15minUVIndexGeoJSON :: IntervalPeriod 15 -> CommonSpatialDataInfrastructurePortalReq JSVal

deriving instance Eq (CommonSpatialDataInfrastructurePortalReq a)

instance Hashable (CommonSpatialDataInfrastructurePortalReq a) where
  hashWithSalt s req = hashWithSalt @Int s $ case req of
    GetLatest15minUVIndexGeoJSON p -> 1 `hashWithSalt` p

deriving instance Show (CommonSpatialDataInfrastructurePortalReq a)

instance ShowP CommonSpatialDataInfrastructurePortalReq where showp = show

instance StateKey CommonSpatialDataInfrastructurePortalReq where
  data State CommonSpatialDataInfrastructurePortalReq = CommonSpatialDataInfrastructurePortalReqState

instance DataSourceName CommonSpatialDataInfrastructurePortalReq where
  dataSourceName _ = pack "CSDI Portal API"

csdiPortalReqToURI :: CommonSpatialDataInfrastructurePortalReq a -> URI
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
  fetch =
    backgroundFetchPar
    -- NOTE: sad boilerplate
    $
      \req -> case req of
        GetLatest15minUVIndexGeoJSON _ -> fetchGetJSON Proxy $ csdiPortalReqToURI req

getLatest15minUVIndexGeoJSON :: UTCTime -> GenHaxl u w JSVal
getLatest15minUVIndexGeoJSON = fetchCacheable . GetLatest15minUVIndexGeoJSON . utcTimeToIntervalPeriod Proxy

data UVIndexRecord = UVIndexRecord TimeData Double
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSVal, FromJSVal)

instance FromNamedRecord UVIndexRecord where
  parseNamedRecord m = do
    fromJulianValid
      <$> m CSV..: "Date time (Year)"
      <*> m CSV..: "Date time (Month)"
      <*> m CSV..: "Date time (Day)"
      >>= \case
        Nothing -> fail "Invalid Day"
        Just day -> do
          timeOfDay <-
            TimeOfDay
              <$> m CSV..: "Date time (Hour)"
              <*> m CSV..: "Date time (Minute)"
              -- sec <- m .: "Date time (Second)"
              <*> pure 0
          tz <-
            stripPrefix "UTC+" <$> m CSV..: "Date time (Time Zone)" >>= \case
              Nothing -> fail "expected prefix: 'UTC+' for 'Date time (Time Zone)'"
              Just i -> case decimal i of
                Left err -> fail err
                Right (i', "") -> pure $ hoursToTimeZone i'
                Right (_, leftover) -> fail $ "unexpected suffix for " <> unpack leftover <> " 'Date time (Time Zone)'"
          idx <- m CSV..: "past 15-minute mean UV Index"
          pure $ UVIndexRecord (TimeData $ ZonedTime (LocalTime day timeOfDay) tz) idx

instance FromRecord UVIndexRecord where
  parseRecord m =
    fromJulianValid <$> m .! 0 <*> m .! 1 <*> m .! 2 >>= \case
      Nothing -> fail "Invalid Day"
      Just day -> do
        timeOfDay <-
          TimeOfDay
            <$> m .! 3
            <*> m .! 4
            -- sec <- m .! 5
            <*> pure 0
        tz <-
          stripPrefix "UTC+" <$> m .! 6 >>= \case
            Nothing -> fail "expected prefix: 'UTC+' for 'Date time (Time Zone)'"
            Just i -> case decimal i of
              Left err -> fail err
              Right (i', "") -> pure $ hoursToTimeZone i'
              Right (_, leftover) -> fail $ "unexpected suffix for " <> unpack leftover <> " 'Date time (Time Zone)'"
        idx <- m .! 7
        pure $ UVIndexRecord (TimeData $ ZonedTime (LocalTime day timeOfDay) tz) idx

instance FromJSON UVIndexRecord where
  parseJSON = withObject "UVIndexRecord" $ \o ->
    UVIndexRecord
      <$> o .: "time"
      <*> o .: "record"
