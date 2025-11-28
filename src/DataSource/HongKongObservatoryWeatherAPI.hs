{-# LANGUAGE TypeFamilies #-}

-- NOTE: https://www.hko.gov.hk/en/abouthko/opendata_intro.htm
module DataSource.HongKongObservatoryWeatherAPI where

import Data.Hashable
import Data.Text hiding (show)
import Data.Time
import Data.Typeable
import Data.Void
import DataSource.HongKongObservatoryWeatherAPI.Types
import DataSource.JSM
import DataSource.LocalStorage
import Haxl.Core hiding (throw)
import Language.Javascript.JSaddle hiding (Object, Success)
import Network.URI
import Utils.Haxl
import Utils.IntervalPeriod
import Utils.Serialise

-- NOTE: Weather Information API
data HKOWeatherInformationReq a where
  GetLocalWeatherForecast :: IntervalPeriod 60 -> HKOWeatherInformationReq LocalWeatherForecast
  Get9DayWeatherForecast :: TwiceADay -> HKOWeatherInformationReq NineDayWeatherForecast
  GetCurrentWeatherReport :: IntervalPeriod 60 -> HKOWeatherInformationReq CurrentWeatherReport
  GetWeatherWarningSummary :: UTCTime -> HKOWeatherInformationReq SerialisableValue
  GetWeatherWarningInfo :: UTCTime -> HKOWeatherInformationReq SerialisableValue
  GetSpecialWeatherTips :: UTCTime -> HKOWeatherInformationReq SerialisableValue

deriving instance Eq (HKOWeatherInformationReq a)

instance Hashable (HKOWeatherInformationReq a) where
  hashWithSalt s req = hashWithSalt @Int s $ case req of
    GetLocalWeatherForecast p -> 0 `hashWithSalt` p
    Get9DayWeatherForecast p -> 1 `hashWithSalt` p
    GetCurrentWeatherReport p -> 2 `hashWithSalt` p
    GetWeatherWarningSummary t -> 3 `hashWithSalt` t
    GetWeatherWarningInfo t -> 4 `hashWithSalt` t
    GetSpecialWeatherTips t -> 5 `hashWithSalt` t

deriving instance Show (HKOWeatherInformationReq a)

instance ShowP HKOWeatherInformationReq where showp = show

instance StateKey HKOWeatherInformationReq where
  newtype State HKOWeatherInformationReq = HKOWeatherInformationReqState JSContextRef

instance DataSourceName HKOWeatherInformationReq where
  dataSourceName _ = pack "HKO Weather Information API"

hkoWeatherInformationReqToURI :: HKOWeatherInformationReq a -> URI
hkoWeatherInformationReqToURI req =
  URI
    "https:"
    (Just $ nullURIAuth {uriRegName = "data.weather.gov.hk"})
    "/weatherAPI/opendata/weather.php"
    ( "?dataType=" <> case req of
        GetLocalWeatherForecast _ -> "flw"
        Get9DayWeatherForecast _ -> "fnd"
        GetCurrentWeatherReport _ -> "rhrread"
        GetWeatherWarningSummary _ -> "warnsum"
        GetWeatherWarningInfo _ -> "warninginfo"
        GetSpecialWeatherTips _ -> "swt"
    )
    ""

instance DataSource u HKOWeatherInformationReq where
  fetch reqState@(HKOWeatherInformationReqState jscontext) =
    backgroundFetchPar
      ( -- NOTE: sad boilerplate
        \req -> runJSaddle jscontext $ case req of
          GetLocalWeatherForecast _ -> fetchGetJSON Proxy $ hkoWeatherInformationReqToURI req
          Get9DayWeatherForecast _ -> fetchGetJSON Proxy $ hkoWeatherInformationReqToURI req
          GetCurrentWeatherReport _ -> fetchGetJSON Proxy $ hkoWeatherInformationReqToURI req
          GetWeatherWarningSummary _ -> fetchGetJSON Proxy $ hkoWeatherInformationReqToURI req
          GetWeatherWarningInfo _ -> fetchGetJSON Proxy $ hkoWeatherInformationReqToURI req
          GetSpecialWeatherTips _ -> fetchGetJSON Proxy $ hkoWeatherInformationReqToURI req
      )
      reqState

-- NOTE: Rainfall in The Past Hour from Automatic Weather Station API
data HKOHourlyRainFallReq a where
  GetHourlyRainFall :: HKOHourlyRainFallReq Void

getLocalWeatherForecast :: UTCTime -> GenHaxl u w LocalWeatherForecast
getLocalWeatherForecast = fetchCacheable . GetLocalWeatherForecast . utcTimeToIntervalPeriod Proxy

get9DayWeatherForecast :: UTCTime -> GenHaxl u w NineDayWeatherForecast
get9DayWeatherForecast = fetchCacheable . Get9DayWeatherForecast . utcTimeToIntervalPeriod Proxy

getCurrentWeatherReport :: UTCTime -> GenHaxl u w CurrentWeatherReport
getCurrentWeatherReport = fetchCacheable . GetCurrentWeatherReport . utcTimeToIntervalPeriod Proxy
