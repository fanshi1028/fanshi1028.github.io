{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- NOTE: https://www.hko.gov.hk/en/abouthko/opendata_intro.htm
module DataSource.CommonSpatialDataInfrastructurePortal where

import Data.Hashable
import Data.Text hiding (show)
import Data.Time
import Data.Typeable
import DataSource.LocalStorage
import GHC.Generics
import Haxl.Core hiding (throw)
import Miso.DSL
import Miso.JSON
import Network.URI
import Utils.Haxl
import Utils.IntervalPeriod
import Utils.JSON ()
import Utils.Time

-- NOTE: CSDI Portal API
data CommonSpatialDataInfrastructurePortalReq a where
  GetLatest15minUVIndexGeoJSON :: IntervalPeriod 15 -> CommonSpatialDataInfrastructurePortalReq JSVal
  GetDistrictBoundary :: IntervalPeriod 1440 -> CommonSpatialDataInfrastructurePortalReq JSVal

deriving instance Eq (CommonSpatialDataInfrastructurePortalReq a)

instance Hashable (CommonSpatialDataInfrastructurePortalReq a) where
  hashWithSalt s req = hashWithSalt @Int s $ case req of
    GetLatest15minUVIndexGeoJSON p -> 1 `hashWithSalt` p
    GetDistrictBoundary p -> 1 `hashWithSalt` p

deriving instance Show (CommonSpatialDataInfrastructurePortalReq a)

instance ShowP CommonSpatialDataInfrastructurePortalReq where showp = show

instance StateKey CommonSpatialDataInfrastructurePortalReq where
  data State CommonSpatialDataInfrastructurePortalReq = CommonSpatialDataInfrastructurePortalReqState

instance DataSourceName CommonSpatialDataInfrastructurePortalReq where
  dataSourceName _ = pack "CSDI Portal API"

csdiPortalReqToURI :: CommonSpatialDataInfrastructurePortalReq a -> URI
csdiPortalReqToURI =
  let commonQuery :: [(StrictText, StrictText)] = [("service", "wfs"), ("request", "GetFeature"), ("outputFormat", "geojson")]
      mkURIHelper dataId query' =
        nullURI
          { uriScheme = "https:",
            uriAuthority = Just $ nullURIAuth {uriRegName = "portal.csdi.gov.hk"},
            uriPath = "/server/services/common/" <> dataId <> "/MapServer/WFSServer",
            uriQuery = renderQueryToString $ commonQuery <> query'
          }
   in \case
        GetLatest15minUVIndexGeoJSON _ ->
          mkURIHelper "hko_rcd_1634894904080_80327" [("typenames", "latest_15min_uvindex"), ("count", "25")]
        GetDistrictBoundary _ ->
          mkURIHelper "had_rcd_1634523272907_75218" [("typenames", "DCD"), ("count", "50")]

instance DataSource u CommonSpatialDataInfrastructurePortalReq where
  fetch =
    backgroundFetchPar
    -- NOTE: sad boilerplate
    $
      \req -> case req of
        GetLatest15minUVIndexGeoJSON _ -> fetchGetJSON Proxy $ csdiPortalReqToURI req
        GetDistrictBoundary _ -> fetchGetJSON Proxy $ csdiPortalReqToURI req

getLatest15minUVIndexGeoJSON :: UTCTime -> GenHaxl u w JSVal
getLatest15minUVIndexGeoJSON = fetchCacheable . GetLatest15minUVIndexGeoJSON . utcTimeToIntervalPeriod Proxy

getDistrictBoundary :: UTCTime -> GenHaxl u w JSVal
getDistrictBoundary = fetchCacheable . GetDistrictBoundary . utcTimeToIntervalPeriod Proxy

data UVIndexRecord = UVIndexRecord TimeData Double
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSVal, FromJSVal)

instance FromJSON UVIndexRecord where
  parseJSON = withObject "UVIndexRecord" $ \o ->
    UVIndexRecord
      <$> o .: "time"
      <*> o .: "record"
