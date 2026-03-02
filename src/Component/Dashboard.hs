{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}

module Component.Dashboard (dashboardComponent) where

import Component.Dashboard.Types
import Component.Dashboard.View
import Component.Foreign.MapLibre
import Control.Monad
import Data.Function
import Data.Maybe
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
import Numeric.Natural
import Text.Read
import Utils.Haxl
import Utils.JS

location :: Lens Model (Maybe (Either GeolocationError Geolocation))
location = lens _location $ \record x -> record {_location = x}

focusedDistrict :: Lens Model (Maybe District)
focusedDistrict = lens _focusedDistrict $ \record x -> record {_focusedDistrict = x}

time :: Lens Model (Maybe UTCTime)
time = lens _time $ \record x -> record {_time = x}

timeSliderValue :: Lens Model Natural
timeSliderValue = lens _timeSliderValue $ \record x -> record {_timeSliderValue = x}

currentWeatherReport :: Lens Model (Maybe CurrentWeatherReport)
currentWeatherReport = lens _currentWeatherReport $ \record x -> record {_currentWeatherReport = x}

localWeatherForecast :: Lens Model (Maybe LocalWeatherForecast)
localWeatherForecast = lens _localWeatherForecast $ \record x -> record {_localWeatherForecast = x}

nineDayWeatherForecast :: Lens Model (Maybe NineDayWeatherForecast)
nineDayWeatherForecast = lens _9DayWeatherForecast $ \record x -> record {_9DayWeatherForecast = x}

displayTemperature :: Lens Model Bool
displayTemperature = lens _displayTemperature $ \record x -> record {_displayTemperature = x}

displayRainfall :: Lens Model Bool
displayRainfall = lens _displayRainfall $ \record x -> record {_displayRainfall = x}

displayWeatherPanel :: Lens Model Bool
displayWeatherPanel = lens _displayWeatherPanel $ \record x -> record {_displayWeatherPanel = x}

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

    getWeatherStations t >>= misoRunAction . AddGeoJSON WeatherStations

    uncachedRequest $ GetSpecialWeatherTips t

  saveCacheToLocalStorage $ dataCache env'

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  NoOp -> pure ()
  InitAction -> sync $ pure FetchWeatherData
  InitMapLibre -> io_ . void $ createMap
  FetchWeatherData -> withSink fetchData
  SetLocation loc -> do
    io_ $ addMarkerAndEaseToLocation loc
    location .= Just (Right loc)
  FocusDistrict (Left district@(District code _ _)) -> do
    io_ $ focusDistrict code
    focusedDistrict .= Just district
  FocusDistrict (Right geoJSON) -> sync $ do
    district <- callMapLibreFunction (ms "getDistrict") [geoJSON]
    fromJSVal district >>= \case
      Nothing -> NoOp <$ consoleError' (ms "getDistrict fromJSVal failed, skipped FocusDistrict", district)
      Just district' -> pure $ FocusDistrict $ Left district'
  SetCurrentTime t -> time .= Just t
  SetTimeSliderValue v -> case readMaybe (fromMisoString v) of
    Nothing -> io_ $ consoleError' ("impossible! unexpected timeSliderValue: %o", v)
    Just v' -> do
      timeSliderValue .= v'
      io $ pure FetchWeatherData
  SetLocalWeatherForecast w -> localWeatherForecast .= Just w
  SetCurrentWeatherReport w -> currentWeatherReport .= Just w
  Set9DayWeatherForecast w -> nineDayWeatherForecast .= Just w
  SetDisplayTemperature b -> displayTemperature .= b
  SetDisplayRainfall b -> displayRainfall .= b
  AddGeoJSON FocusedDistrictBoundary geoJSON -> io_ . void $ callMapLibreFunctionWithMap (ms "addDistrictBoundaryLayer") geoJSON
  AddGeoJSON WeatherStations geoJSON -> io_ . void $ callMapLibreFunctionWithMap (ms "addWeatherStationsLayer") geoJSON
  ToggleDisplayHardSurfaceSoccerPitch7 -> io_ toggle_hssp7
  ToggleDisplayWeatherPanel ->
    displayWeatherPanel <%= not >>= \case
      True -> io $ pure FetchWeatherData
      False -> pure ()

dashboardComponent :: Component parent Model Action
dashboardComponent = (component defaultModel updateModel viewModel) {mount = Just InitAction}
