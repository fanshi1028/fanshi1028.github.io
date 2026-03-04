{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Component.Dashboard.Types where

import Data.Maybe
import Data.Time
import DataSource.CommonSpatialDataInfrastructurePortal
import DataSource.HongKongObservatoryWeatherAPI.Types
import Miso
import Miso.Navigator
import Numeric.Natural
import Utils.JS ()
import Prelude hiding (show)

data GeoJSONDataId = FocusedDistrictBoundary | WeatherStations
  deriving stock (Eq, Show)

data Model
  = Model
  { _time :: Maybe UTCTime,
    _timeSliderValue :: Natural,
    _location :: Maybe (Either GeolocationError Geolocation),
    _focusedDistrict :: Maybe District,
    _currentWeatherReport :: Maybe CurrentWeatherReport,
    _localWeatherForecast :: Maybe LocalWeatherForecast,
    _9DayWeatherForecast :: Maybe NineDayWeatherForecast,
    _displayWeatherPanel :: Bool,
    _displayRainfall :: Bool,
    _displayTemperature :: Bool
  }
  deriving (Eq)

data Action
  = NoOp
  | InitAction
  | FetchWeatherData
  | InitMapLibre
  | ClearLocation
  | FindAndSetLocation
  | SetLocation Geolocation
  | FocusDistrict District
  | SetCurrentTime UTCTime
  | SetTimeSliderValue MisoString
  | SetCurrentWeatherReport CurrentWeatherReport
  | SetLocalWeatherForecast LocalWeatherForecast
  | Set9DayWeatherForecast NineDayWeatherForecast
  | AddGeoJSON GeoJSONDataId JSVal
  | SetDisplayTemperature Bool
  | SetDisplayRainfall Bool
  | ToggleDisplayHardSurfaceSoccerPitch7
  | ToggleDisplayWeatherPanel
  deriving stock (Eq, Show)

defaultModel :: Model
defaultModel = Model Nothing 0 Nothing Nothing Nothing Nothing Nothing False False False
