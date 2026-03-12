{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Component.Dashboard (dashboardComponent) where

import Component.Dashboard.Types
import Component.Dashboard.View
import Component.Foreign.MapLibre
import Control.Monad
import Data.Function
import Data.Time
import DataSource.BrowserGeolocationAPI
import DataSource.CommonSpatialDataInfrastructurePortal
import DataSource.HongKongObservatoryWeatherAPI
import DataSource.HongKongObservatoryWeatherAPI.Types
import DataSource.LocalStorage
import DataSource.MisoRun
import DataSource.SimpleFetch
import Haxl.Core
import Miso
import Miso.JSON hiding ((.=))
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

ifInHK :: Lens Model IfInHK
ifInHK = lens _ifInHK $ \record x -> record {_ifInHK = x}

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

defaultStateStore :: StateStore
defaultStateStore =
  stateEmpty
    & stateSet LocationReqState
    & stateSet HKOWeatherInformationReqState
    & stateSet CommonSpatialDataInfrastructurePortalReqState
    & stateSet JSMActionState
    & stateSet (LocalStorageReqState @HKOWeatherInformationReq)
    & stateSet (LocalStorageReqState @SimpleFetch)
    & stateSet (LocalStorageReqState @CommonSpatialDataInfrastructurePortalReq)

initFetchData :: UTCTime -> GenHaxl () () ()
initFetchData t = void $ do
  misoRunAction $ SetCurrentTime t

  getDistrictBoundary t >>= misoRunAction . AddGeoJSON FocusedDistrictBoundary

  getCurrentWeatherReport t >>= misoRunAction . SetCurrentWeatherReport

  getLocalWeatherForecast t >>= misoRunAction . SetLocalWeatherForecast

  get9DayWeatherForecast t >>= misoRunAction . Set9DayWeatherForecast

  misoRunAction FindAndSetLocation

  uncachedRequest $ GetWeatherWarningSummary t
  uncachedRequest $ GetWeatherWarningInfo t

  getWeatherStations t >>= misoRunAction . AddGeoJSON WeatherStations

  uncachedRequest $ GetSpecialWeatherTips t

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  NoOp -> pure ()
  InitAction -> sync $ pure FetchWeatherData
  InitMapLibre -> io_ . void $ createMap
  FetchWeatherData -> withSink $ \sink ->
    getCurrentTime
      >>= misoRunGenHaxl defaultStateStore sink . initFetchData
  ClearLocation -> do
    io_ . void $ callMapLibreFunction "clearLocation" ()
    ifInHK .= NotInHK
    location .= Nothing
    focusedDistrict .= Nothing
  FindAndSetLocation -> withSink $ \sink ->
    misoRunGenHaxl defaultStateStore sink $ do
      loc <- uncachedRequest GetCurrentPosition
      misoRunAction $ SetLocation loc
      getDistrictByLocation loc >>= \case
        Nothing -> misoRunAction $ SetIfInHK NotInHK
        Just district -> do
          misoRunAction $ SetIfInHK InHK
          misoRunAction $ FocusDistrict district
  SetLocation loc -> do
    io_ $ addLocationMarkerAndEaseToLocation loc
    location .= Just (Right loc)
  FocusDistrict district@(District code _ _) -> do
    io_ $ focusDistrict code
    focusedDistrict .= Just district
  SetIfInHK hk -> do
    ifInHK .= hk
    case hk of
      NotInHKButPretendYouAre -> io_ . void $ callMapLibreFunction "zoomToHK" ()
      _ -> pure ()
  SetCurrentTime t -> time .= Just t
  SetTimeSliderValue v -> case readMaybe (fromMisoString v) of
    Nothing -> io_ $ consoleError' ("impossible! unexpected timeSliderValue: %o" :: MisoString, v)
    Just v' -> do
      timeSliderValue .= v'
      io $ pure FetchWeatherData
  SetLocalWeatherForecast w -> localWeatherForecast .= Just w
  SetCurrentWeatherReport w -> currentWeatherReport .= Just w
  Set9DayWeatherForecast w -> nineDayWeatherForecast .= Just w
  SetDisplayTemperature b -> displayTemperature .= b
  SetDisplayRainfall b -> displayRainfall .= b
  AddGeoJSON FocusedDistrictBoundary geoJSON ->
    withSink $ \sink -> do
      focusDistrictCb <-
        asyncCallback1 $ \v ->
          fromJSVal_Value v >>= \case
            Nothing -> consoleError' ("impossible district data: %o" :: MisoString, v)
            Just v' -> case parseEither parseJSON v' of
              Left err -> consoleError' ("impossible district data (%s): %o" :: MisoString, err, v)
              Right r -> sink $ FocusDistrict r
      void $ callMapLibreFunctionWithMap2 "addDistrictBoundaryLayer" geoJSON focusDistrictCb
  AddGeoJSON WeatherStations _geoJSON -> io_ . void $ do
    -- callMapLibreFunctionWithMap "addWeatherStationsLayer" geoJSON
    -- TEMP FIXME
    pure ()
  ToggleDisplayHardSurfaceSoccerPitch7 -> io_ toggle_hssp7
  ToggleDisplayWeatherPanel ->
    displayWeatherPanel <%= not >>= \case
      True -> io $ pure FetchWeatherData
      False -> pure ()

dashboardComponent :: Component parent Model Action
dashboardComponent = (component defaultModel updateModel viewModel) {mount = Just InitAction}
