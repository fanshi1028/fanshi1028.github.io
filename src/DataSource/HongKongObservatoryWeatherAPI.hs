{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- NOTE: https://www.hko.gov.hk/en/abouthko/opendata_intro.htm
module DataSource.HongKongObservatoryWeatherAPI where

import Control.Exception (toException)
import Data.Csv
import Data.Hashable
import Data.Text hiding (show)
import Data.Time
import Data.Typeable
import Data.Vector
import Data.Void
import DataSource.HongKongObservatoryWeatherAPI.Types
import DataSource.LocalStorage
import Haxl.Core hiding (throw)
import Language.Javascript.JSaddle hiding (Object, Success)
import Network.URI
import Network.URI.Static
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
  --
  GetLatest15minUVIndex :: IntervalPeriod 15 -> HKOWeatherInformationReq (Vector (UTCTime, Double))

deriving instance Eq (HKOWeatherInformationReq a)

instance Hashable (HKOWeatherInformationReq a) where
  hashWithSalt s req = hashWithSalt @Int s $ case req of
    GetLocalWeatherForecast p -> 0 `hashWithSalt` p
    Get9DayWeatherForecast p -> 1 `hashWithSalt` p
    GetCurrentWeatherReport p -> 2 `hashWithSalt` p
    GetWeatherWarningSummary t -> 3 `hashWithSalt` t
    GetWeatherWarningInfo t -> 4 `hashWithSalt` t
    GetSpecialWeatherTips t -> 5 `hashWithSalt` t
    GetLatest15minUVIndex p -> 6 `hashWithSalt` p

deriving instance Show (HKOWeatherInformationReq a)

instance ShowP HKOWeatherInformationReq where showp = show

instance StateKey HKOWeatherInformationReq where
  newtype State HKOWeatherInformationReq = HKOWeatherInformationReqState JSContextRef

instance DataSourceName HKOWeatherInformationReq where
  dataSourceName _ = pack "HKO Weather Information API"

hkoWeatherInformationReqToURI :: HKOWeatherInformationReq a -> URI
hkoWeatherInformationReqToURI (GetLatest15minUVIndex _) = [uri|https://data.weather.gov.hk/weatherAPI/hko_data/regional-weather/latest_15min_uvindex.csv|]
hkoWeatherInformationReqToURI req =
  [uri|https://data.weather.gov.hk/weatherAPI/opendata/weather.php|]
    { uriQuery =
        renderQueryTextToString
          [ ( pack "dataType",
              Just . pack $ case req of
                GetLocalWeatherForecast _ -> "flw"
                Get9DayWeatherForecast _ -> "fnd"
                GetCurrentWeatherReport _ -> "rhrread"
                GetWeatherWarningSummary _ -> "warnsum"
                GetWeatherWarningInfo _ -> "warninginfo"
                GetSpecialWeatherTips _ -> "swt"
            )
          ]
    }

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
          GetLatest15minUVIndex _ -> do
            fetchGetCSV Proxy HasHeader (corsProxy $ hkoWeatherInformationReqToURI req) >>= \case
              Left err -> pure $ Left err
              Right v -> case traverse (\(str, uvIdx) -> (,uvIdx) <$> parseTimeM False defaultTimeLocale "%Y%m%d%H%M" str) v of
                Nothing -> pure . Left . toException $ MonadFail $ pack "TEMP FIXME: fail to parse time"
                Just v' -> pure $ Right v'
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

getLatest15minUVIndex :: UTCTime -> GenHaxl u w (Vector (UTCTime, Double))
getLatest15minUVIndex = fetchCacheable . GetLatest15minUVIndex . utcTimeToIntervalPeriod Proxy
