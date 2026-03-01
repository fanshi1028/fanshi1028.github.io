{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Component.Dashboard.Types where

import Component.Foreign.MapLibre
import Component.Popover
import Data.Function
import Data.Interval
import Data.List
import Data.List.NonEmpty
import Data.Maybe
import Data.Scientific as SCI
import Data.Text hiding (find, foldl')
import Data.Time
import DataSource.HongKongObservatoryWeatherAPI.Types
import GHC.Generics
import Miso
import Miso.Html.Element
import Miso.Html.Event
import Miso.Html.Property hiding (label_)
import Miso.Navigator
import Numeric.Natural
import Numeric.Units.Dimensional hiding ((*), (-))
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.SIUnits hiding (toDegreeCelsiusAbsolute)
import Utils.Dimensional
import Utils.JS ()
import Utils.Time
import View.SVG.LoadSpinner
import Prelude hiding (show)

data District = District
  { _AREA_CODE :: MisoString,
    _NAME_EN :: MisoString,
    _NAME_TC :: MisoString
  }
  deriving stock (Eq, Show, Generic)

instance FromJSVal District where
  fromJSVal v = do
    mAreaCode <- v ! "AREA_CODE" >>= fromJSVal
    mNameEN <- v ! "NAME_EN" >>= fromJSVal
    mNameTC <- v ! "NAME_TC" >>= fromJSVal
    pure $ District <$> mAreaCode <*> mNameEN <*> mNameTC

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
  | SetLocation Geolocation
  | FocusDistrict (Either District JSVal)
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
