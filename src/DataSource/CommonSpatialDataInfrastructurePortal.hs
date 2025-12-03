{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- NOTE: https://www.hko.gov.hk/en/abouthko/opendata_intro.htm
module DataSource.CommonSpatialDataInfrastructurePortal where

import Data.Hashable
import Data.Text hiding (show)
import Data.Time
import Data.Typeable
import DataSource.LocalStorage
import Haxl.Core hiding (throw)
import Language.Javascript.JSaddle hiding (Object, Success)
import Network.URI
import Network.URI.Static
import Text.XML.Light
import Utils.Haxl
import Utils.IntervalPeriod
import Utils.Serialise

-- NOTE: CSDI Portal API
data CommonSpatialDataInfrastructurePortalReq a where
  GetLatest15minUVIndex :: IntervalPeriod 15 -> CommonSpatialDataInfrastructurePortalReq SerialisableValue

deriving instance Eq (CommonSpatialDataInfrastructurePortalReq a)

instance Hashable (CommonSpatialDataInfrastructurePortalReq a) where
  hashWithSalt s req = hashWithSalt @Int s $ case req of
    GetLatest15minUVIndex p -> 0 `hashWithSalt` p

deriving instance Show (CommonSpatialDataInfrastructurePortalReq a)

instance ShowP CommonSpatialDataInfrastructurePortalReq where showp = show

instance StateKey CommonSpatialDataInfrastructurePortalReq where
  newtype State CommonSpatialDataInfrastructurePortalReq = CommonSpatialDataInfrastructurePortalReqState JSContextRef

instance DataSourceName CommonSpatialDataInfrastructurePortalReq where
  dataSourceName _ = pack "CSDI Portal API"

csdiPortalReqToURI :: CommonSpatialDataInfrastructurePortalReq a -> URI
csdiPortalReqToURI (GetLatest15minUVIndex _) =
  [uri|https://portal.csdi.gov.hk/server/services/common/hko_rcd_1634894904080_80327/MapServer/WFSServer|]
    { uriQuery =
        renderQueryTextToString
          [ (pack "service", Just $ pack "wfs"),
            (pack "request", Just $ pack "GetFeature"),
            (pack "typenames", Just $ pack "latest_15min_uvindex"),
            (pack "outputFormat", Just $ pack "geojson"),
            (pack "maxFeatures", Just $ pack "10"),
            (pack "srsName", Just $ pack "EPSG:4326"),
            ( pack "filter",
              Just . pack . showElement $
                -- "<Filter><Intersects><PropertyName>SHAPE</PropertyName><gml:Envelope srsName='EPSG:4326'><gml:lowerCorner>22.15 113.81</gml:lowerCorner><gml:upperCorner>22.62 114.45</gml:upperCorner></gml:Envelope></Intersects></Filter>"
                unode "Filter" $
                  [ unode "Intersects" $
                      [ unode "PropertyName" "SHAPE",
                        unode "gml:Envelope" $
                          ( [Attr (unqual "srsName") "EPSG:4326"],
                            [unode "gml:lowerCorner" "22.15 113.81", unode "gml:upperCorner" "22.62 114.45"]
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
          GetLatest15minUVIndex _ -> fetchGetJSON Proxy $ csdiPortalReqToURI req
      )
      reqState

getLatest15minUVIndex :: UTCTime -> GenHaxl u w SerialisableValue
getLatest15minUVIndex = fetchCacheable . GetLatest15minUVIndex . utcTimeToIntervalPeriod Proxy
