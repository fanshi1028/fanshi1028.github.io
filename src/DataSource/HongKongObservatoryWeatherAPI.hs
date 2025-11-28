{-# LANGUAGE TypeFamilies #-}

module DataSource.HongKongObservatoryWeatherAPI where

import Control.Lens.Setter
import Data.Function
import Data.Hashable
import Data.Text hiding (show)
import Data.Time
import Data.Typeable
import Data.Void
import DataSource.HongKongObservatoryWeatherAPI.Types
import DataSource.JSM
import Haxl.Core hiding (throw)
import Language.Javascript.JSaddle hiding (Object, Success)
import Network.URI
import Network.URI.Lens
import Utils.Haxl
import Utils.Serialise

-- NOTE: Weather Information API
data HKOWeatherInformationReq a where
  GetLocalWeatherForecast :: Day -> HKOWeatherInformationReq LocalWeatherForecast
  Get9DayWeatherForecast :: Day -> HKOWeatherInformationReq NineDayWeatherForecast
  GetCurrentWeatherReport :: Day -> HKOWeatherInformationReq CurrentWeatherReport
  GetWeatherWarningSummary :: Day -> HKOWeatherInformationReq SerialisableValue
  GetWeatherWarningInfo :: Day -> HKOWeatherInformationReq SerialisableValue
  GetSpecialWeatherTips :: Day -> HKOWeatherInformationReq SerialisableValue

deriving instance Eq (HKOWeatherInformationReq a)

instance Hashable (HKOWeatherInformationReq a) where
  hashWithSalt s req = hashWithSalt @Int s $ case req of
    GetLocalWeatherForecast day' -> 0 `hashWithSalt` day'
    Get9DayWeatherForecast day' -> 1 `hashWithSalt` day'
    GetCurrentWeatherReport day' -> 2 `hashWithSalt` day'
    GetWeatherWarningSummary day' -> 3 `hashWithSalt` day'
    GetWeatherWarningInfo day' -> 4 `hashWithSalt` day'
    GetSpecialWeatherTips day' -> 5 `hashWithSalt` day'

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
    (Just $ nullURIAuth & uriRegNameLens .~ "data.weather.gov.hk")
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
