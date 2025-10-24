{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Dashboard (dashboardComponent) where

import Control.Monad.IO.Class
import Dashboard.DataSource.BrowserGeolocationAPI
import Dashboard.DataSource.HongKongObservatoryWeatherAPI
import Dashboard.DataSource.LocalStorage
import Dashboard.DataSource.MisoRun
import Data.Function
import Data.Functor
import Data.Text
import Data.Time
import Haxl.Core
import Language.Javascript.JSaddle
import Miso hiding (URI, getLocalStorage, setLocalStorage)
import Miso.Html.Element
import Miso.Html.Event
import Miso.Navigator
import Prelude hiding (show)

haxlEnvflags :: Flags
haxlEnvflags =
  defaultFlags
#ifndef PRODUCTION
    { trace = 3,
      report = profilingReportFlags
    }
#endif

data Model
  = Model
  { _location :: Maybe (Either GeolocationError Geolocation),
    _currentWeatherReport :: Maybe CurrentWeatherReport,
    _localWeatherForecast :: Maybe LocalWeatherForecast,
    _9DayWeatherForecast :: Maybe NineDayWeatherForecast
  }
  deriving (Eq)

data Action
  = FetchWeatherData
  | SetLocation Geolocation
  | SetCurrentWeatherReport CurrentWeatherReport
  | SetLocalWeatherForecast LocalWeatherForecast
  | Set9DayWeatherForecast NineDayWeatherForecast
  deriving stock (Eq, Show)

defaultModel :: Model
defaultModel = Model Nothing Nothing Nothing Nothing

fetchDataSub :: Sink Action -> JSM ()
fetchDataSub sink = do
  jscontext <- askJSM
  liftIO $ do
    let st =
          stateEmpty
            & stateSet (MisoRunActionState jscontext sink)
            & stateSet (MisoRunJSMState jscontext)
            & stateSet (LocationReqState jscontext)
            & stateSet (HKOWeatherInformationReqState jscontext)
            & stateSet (LocalStorageReqState jscontext)

    env' <- initEnv @() st jscontext

    t <- getCurrentTime

    runHaxl (env' {flags = haxlEnvflags}) $ do
      fromLocalStorageOrDatafetch GetLocalWeatherForecast (\r -> t `diffUTCTime` r.updateTime <= 60 * 15)
        >>= misoRunAction . SetLocalWeatherForecast

      fromLocalStorageOrDatafetch Get9DayWeatherForecast (\r -> t `diffUTCTime` r.updateTime <= 60 * 60 * 12)
        >>= misoRunAction . Set9DayWeatherForecast

      uncachedRequest GetCurrentPosition >>= misoRunAction . SetLocation

      fromLocalStorageOrDatafetch GetCurrentWeatherReport (\r -> t `diffUTCTime` r.updateTime <= 60 * 15)
        >>= misoRunAction . SetCurrentWeatherReport

      fromLocalStorageOrDatafetch GetWeatherWarningSummary $ const True
      fromLocalStorageOrDatafetch GetWeatherWarningInfo $ const True
      fromLocalStorageOrDatafetch GetSpecialWeatherTips $ const True
      pure ()

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  FetchWeatherData -> startSub @Text "FetchWeatherData" fetchDataSub
  SetLocation location -> modify $ \m -> m {_location = Just (Right location)}
  SetLocalWeatherForecast w -> modify $ \m -> m {_localWeatherForecast = Just w}
  SetCurrentWeatherReport w -> modify $ \m -> m {_currentWeatherReport = Just w}
  Set9DayWeatherForecast w -> modify $ \m -> m {_9DayWeatherForecast = Just w}

viewCurrentWeatherReport :: CurrentWeatherReport -> View Model Action
viewCurrentWeatherReport _ = div_ [] ["FIXME TEMP CurrentWeatherReport view not implemented"]

viewLocalWeatherForecast :: LocalWeatherForecast -> View Model Action
viewLocalWeatherForecast _ = div_ [] ["FIXME TEMP LocalWeatherForecast view not implemented"]

view9DayWeatherForecast :: NineDayWeatherForecast -> View Model Action
view9DayWeatherForecast _ = div_ [] ["FIXME TEMP 9DayWeatherForecast view not implemented"]

viewModel :: Model -> View Model Action
viewModel (Model mELocation mCurrentWeatherReport mLocalWeatherForecast m9DayWeatherForecast) =
  div_
    []
    [ case mELocation of
        Nothing -> p_ [] [text "No location data: loading"]
        Just (Right location) -> p_ [] [text $ "you are currently at: " <> ms (show location)]
        Just (Left (GeolocationError errCode err)) -> p_ [] [text $ "location error: " <> ms (show errCode) <> ", " <> err],
      -- case  errCode of
      --   PERMISSION_DENIED -> _
      --   POSITION_UNAVAILABLE -> _
      --   TIMEOUT -> "timeout while getting your location"
      maybe (div_ [] ["FIXME TEMP no CurrentWeatherReport"]) viewCurrentWeatherReport mCurrentWeatherReport,
      maybe (div_ [] ["FIXME TEMP no LocalWeatherForecast"]) viewLocalWeatherForecast mLocalWeatherForecast,
      maybe (div_ [] ["FIXME TEMP no NineDayWeatherForecast"]) view9DayWeatherForecast m9DayWeatherForecast,
      div_
        []
        [ button_ [onClick FetchWeatherData] [text "TEMP FIXME Test: refetch"]
        ]
    ]

dashboardComponent :: Component parent Model Action
dashboardComponent = (component defaultModel updateModel viewModel) {initialAction = Just FetchWeatherData}
