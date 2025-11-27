{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Dashboard (dashboardComponent) where

import Control.Monad.IO.Class
import Dashboard.DataSource.BrowserGeolocationAPI
import Dashboard.DataSource.HongKongObservatoryWeatherAPI
import Dashboard.DataSource.HongKongObservatoryWeatherAPI.Types
import Dashboard.DataSource.IO
import Dashboard.DataSource.JSM
import Dashboard.DataSource.LocalStorage
import Dashboard.View
import Data.Function
import Data.Time
import Haxl.Core
import Haxl.DataSource.ConcurrentIO
import Haxl.LocalStorage
import Language.Javascript.JSaddle
import MapLibre
import Miso
import Miso.Lens
import Miso.Navigator
import Utils.Haxl

haxlEnvflags :: Flags
haxlEnvflags =
  defaultFlags
#ifndef PRODUCTION
    { trace = 0,
      report = profilingReportFlags
    }
#endif

location :: Lens Model (Maybe (Either GeolocationError Geolocation))
location = lens _location $ \record x -> record {_location = x}

timeZone :: Lens Model (Maybe TimeZone)
timeZone = lens _timeZone $ \record x -> record {_timeZone = x}

currentWeatherReport :: Lens Model (Maybe CurrentWeatherReport)
currentWeatherReport = lens _currentWeatherReport $ \record x -> record {_currentWeatherReport = x}

localWeatherForecast :: Lens Model (Maybe LocalWeatherForecast)
localWeatherForecast = lens _localWeatherForecast $ \record x -> record {_localWeatherForecast = x}

nineDayWeatherForecast :: Lens Model (Maybe NineDayWeatherForecast)
nineDayWeatherForecast = lens _9DayWeatherForecast $ \record x -> record {_9DayWeatherForecast = x}

displayTemperature, displayRainfall :: Lens Model Bool
displayTemperature = lens _displayTemperature $ \record x -> record {_displayTemperature = x}
displayRainfall = lens _displayRainfall $ \record x -> record {_displayRainfall = x}

fetchData :: Sink Action -> JSM ()
fetchData sink = do
  jscontext <- askJSM

  ioState <- liftIO mkConcurrentIOState
  let st =
        stateEmpty
          & stateSet (LocationReqState jscontext)
          & stateSet (HKOWeatherInformationReqState jscontext)
          & stateSet (JSMActionState jscontext)
          & stateSet (LocalStorageReqState @HKOWeatherInformationReq jscontext)
          -- TEMP FIXME JSMActin in general should not be cached, but only when we fetch url, and that is exactly how we are abusing it.
          & stateSet (LocalStorageReqState @JSMAction jscontext)
          & stateSet ioState

  env' <- liftIO $ initEnv @() st jscontext

  _ <- liftIO . runHaxl (env' {flags = haxlEnvflags}) $ do
    let misoRunAction' = misoRunAction jscontext sink
    tdy@(pred -> ytd) <- utctDay <$> uncachedRequest GetCurrentTime

    uncachedRequest GetCurrentTimeZone >>= misoRunAction' . SetTimeZone

    fetchCacheable (GetLocalWeatherForecast tdy) >>= misoRunAction' . SetLocalWeatherForecast

    fetchCacheable (Get9DayWeatherForecast tdy) >>= misoRunAction' . Set9DayWeatherForecast

    uncachedRequest GetCurrentPosition >>= misoRunAction' . SetLocation

    fetchCacheable (GetCurrentWeatherReport tdy) >>= misoRunAction' . SetCurrentWeatherReport

    fetchCacheable $ GetWeatherWarningSummary tdy

    fetchCacheable $ GetWeatherWarningInfo tdy

    fetchCacheable $ GetSpecialWeatherTips tdy

  saveCacheToLocalStorage $ dataCache env'

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  InitAction -> issue FetchWeatherData
  InitMapLibre -> io_ $ runMapLibre createMap
  CleanUpMapLibre -> io_ cleanUpMap
  FetchWeatherData -> withSink fetchData
  SetLocation loc -> do
    io_ $ addMarkerAndEaseToLocation loc
    location .= Just (Right loc)
  SetTimeZone tz -> timeZone .= Just tz
  SetLocalWeatherForecast w -> localWeatherForecast .= Just w
  SetCurrentWeatherReport w -> currentWeatherReport .= Just w
  Set9DayWeatherForecast w -> nineDayWeatherForecast .= Just w
  SetDisplayTemperature b -> displayTemperature .= b
  SetDisplayRainfall b -> displayRainfall .= b

dashboardComponent :: Component parent Model Action
dashboardComponent = (component defaultModel updateModel viewModel) {initialAction = Just InitAction}
