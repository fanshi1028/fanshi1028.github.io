{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Component.Dashboard (dashboardComponent) where

import Component.Dashboard.View
import Component.Foreign.MapLibre
import Data.Function
import Data.Text
import Data.Time
import DataSource.BrowserGeolocationAPI
import DataSource.CommonSpatialDataInfrastructurePortal
import DataSource.HongKongObservatoryWeatherAPI
import DataSource.HongKongObservatoryWeatherAPI.Types
import DataSource.IO
import DataSource.LocalStorage
import DataSource.MisoRun
import DataSource.SimpleFetch
import Haxl.Core
import Haxl.DataSource.ConcurrentIO
import Miso
import Miso.Lens hiding ((*~))
import Miso.Navigator

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

focusedDistrict :: Lens Model (Maybe StrictText)
focusedDistrict = lens _focusedDistrict $ \record x -> record {_focusedDistrict = x}

time :: Lens Model (Maybe UTCTime)
time = lens _time $ \record x -> record {_time = x}

currentWeatherReport :: Lens Model (Maybe CurrentWeatherReport)
currentWeatherReport = lens _currentWeatherReport $ \record x -> record {_currentWeatherReport = x}

localWeatherForecast :: Lens Model (Maybe LocalWeatherForecast)
localWeatherForecast = lens _localWeatherForecast $ \record x -> record {_localWeatherForecast = x}

nineDayWeatherForecast :: Lens Model (Maybe NineDayWeatherForecast)
nineDayWeatherForecast = lens _9DayWeatherForecast $ \record x -> record {_9DayWeatherForecast = x}

displayTemperature, displayRainfall :: Lens Model Bool
displayTemperature = lens _displayTemperature $ \record x -> record {_displayTemperature = x}
displayRainfall = lens _displayRainfall $ \record x -> record {_displayRainfall = x}

fetchData :: Sink Action -> IO ()
fetchData sink = do
  ioState <- mkConcurrentIOState
  let st =
        stateEmpty
          & stateSet (MisoRunActionState sink)
          & stateSet LocationReqState
          & stateSet HKOWeatherInformationReqState
          & stateSet CommonSpatialDataInfrastructurePortalReqState
          & stateSet JSMActionState
          & stateSet (LocalStorageReqState @HKOWeatherInformationReq)
          & stateSet (LocalStorageReqState @SimpleFetch)
          & stateSet (LocalStorageReqState @CommonSpatialDataInfrastructurePortalReq)
          & stateSet ioState

  env' <- initEnv @() st ()

  _ <- runHaxl (env' {flags = haxlEnvflags}) $ do
    t <- uncachedRequest GetCurrentTime

    misoRunAction $ SetCurrentTime t

    getDistrictBoundary t >>= misoRunAction . AddGeoJSON FocusedDistrictBoundary

    getLocalWeatherForecast t >>= misoRunAction . SetLocalWeatherForecast

    get9DayWeatherForecast t >>= misoRunAction . Set9DayWeatherForecast

    loc <- uncachedRequest GetCurrentPosition
    misoRunAction $ SetLocation loc

    getDistrictByLocation loc >>= misoRunAction . FocusDistrict . Right

    getCurrentWeatherReport t >>= misoRunAction . SetCurrentWeatherReport

    uncachedRequest $ GetWeatherWarningSummary t
    uncachedRequest $ GetWeatherWarningInfo t


    uncachedRequest $ GetSpecialWeatherTips t

  saveCacheToLocalStorage $ dataCache env'

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  InitAction -> issue FetchWeatherData
  InitMapLibre -> io_ $ runMapLibre createMap
  FetchWeatherData -> withSink fetchData
  SetLocation loc -> do
    io_ . runMapLibre $ addMarkerAndEaseToLocation loc
    location .= Just (Right loc)
  FocusDistrict toFocus -> io_ . runMapLibre $ focusDistrict toFocus -- TEMP FIXME setFocusDistrict!
  SetCurrentTime t -> time .= Just t
  SetLocalWeatherForecast w -> localWeatherForecast .= Just w
  SetCurrentWeatherReport w -> currentWeatherReport .= Just w
  Set9DayWeatherForecast w -> nineDayWeatherForecast .= Just w
  SetDisplayTemperature b -> displayTemperature .= b
  SetDisplayRainfall b -> displayRainfall .= b
  AddGeoJSON FocusedDistrictBoundary geoJSON -> io_ . runMapLibre $ addDistrictBoudaryLayer geoJSON
  ToggleDisplayHardSurfaceSoccerPitch7 -> io_ . runMapLibre $ toggle_hssp7

dashboardComponent :: Component parent Model Action
dashboardComponent = (component defaultModel updateModel viewModel) {mount = Just InitAction}
