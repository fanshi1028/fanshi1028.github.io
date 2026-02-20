{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module DataSource.HongKongObservatoryWeatherAPI.Types where

import Control.Applicative
import Data.Functor
import Data.Interval
import Data.List.NonEmpty
import Data.Scientific
import Data.Time
import GHC.Generics
import Miso.DSL hiding (Object)
import Miso.JSON
import Miso.String
import Numeric.Natural
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.SIUnits hiding (fromDegreeCelsiusAbsolute, toDegreeCelsiusAbsolute)
import Utils.Dimensional
import Utils.JSON ()
import Utils.Time

data LocalWeatherForecast = LocalWeatherForecast
  { generalSituation :: MisoString, -- General Situation
    tcInfo :: MisoString, -- Tropical Cyclone Information
    fireDangerWarning :: MisoString, -- Fire Danger Warning Message
    forecastPeriod :: MisoString, -- Forecast Period
    forecastDesc :: MisoString, -- Forecast Description
    outlook :: MisoString, -- Outlook
    updateTime :: TimeData -- Update Time YYYY-MM-DD'T'hh:mm:ssZ Example: 2020-09-01T08:19:00+08:00
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSVal, FromJSVal)

data SoilTemp = SoilTemp
  { place :: MisoString, -- location
    value :: ThermodynamicTemperature Scientific, -- value
    recordTime :: TimeData, -- record time YYYY-MMDD'T'hh:mm:ssZ Example: 2020-09- 01T08:19:00+08:00
    depth :: Length Scientific
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSVal, FromJSVal)

instance FromJSON SoilTemp where
  parseJSON = withObject "SoilTemp" $ \o -> do
    place <- o .: "place"
    temp <- parseJSON_DegreeCelsius $ Object o
    recordTime <- o .: "recordTime"
    depth <-
      o .: "depth"
        >>= parseJSON_Unit
          ( \case
              "metre" -> Right meter
              _ -> Left "'metre' as unit"
          )
    pure $ SoilTemp place temp recordTime depth

data SeaTemp = SeaTemp
  { place :: MisoString, -- location
    value :: ThermodynamicTemperature Scientific, -- value
    recordTime :: TimeData -- record time YYYY-MMDD'T'hh:mm:ssZ Example: 2020-09- 01T08:19:00+08:00
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSVal, FromJSVal)

instance FromJSON SeaTemp where
  parseJSON = withObject "SeaTemp" $ \o ->
    SeaTemp
      <$> o .: "place"
      <*> parseJSON_DegreeCelsius (Object o)
      <*> o .: "recordTime"

data WeatherForecast = WeatherForecast
  { forecastDate :: Day, -- Forecast Date YYYYMMDD
    week :: DayOfWeek, -- Week
    forecastWind :: MisoString, -- Forecast Wind
    forecastWeather :: MisoString, -- Forecast Weather
    forecastTempInterval :: Interval (ThermodynamicTemperature Scientific), -- Forecast Temperature
    forecastRHInterval :: Interval (Dimensionless Scientific), -- Forecast  Relative Humidity
    ------------------------------------------------------------------------------------------------
    -- Probability of Significant Rain Response value:                                            --
    --    High                                                                                    --
    --    Medium High                                                                             --
    --    Medium                                                                                  --
    --    Medium Low                                                                              --
    --    Low                                                                                     --
    -- Response value description: https://www.hko.gov.hk/en/wxinfo/currwx/fnd.htm?tablenote=true --
    ------------------------------------------------------------------------------------------------
    psr :: MisoString, -- TEMP FIXME
    forecastIcon :: Natural
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSVal, FromJSVal)

parseJSON_Unit :: (FromJSON a, Num a) => (MisoString -> (Either MisoString (Unit m d a))) -> Value -> Parser (Quantity d a)
parseJSON_Unit parseUnit = withObject "value with unit" $ \v -> do
  value <- v .: "value"
  parseUnit <$> v .: "unit" >>= \case
    Left err -> typeMismatch (ms err) $ Object v
    Right unit -> pure $ value *~ unit

parseJSON_DegreeCelsius :: (FromJSON a, Fractional a) => Value -> Parser (ThermodynamicTemperature a)
parseJSON_DegreeCelsius = withObject "value with DegreeCelsius" $ \o ->
  o .: "unit" >>= \case
    ("C" :: MisoString) ->
      o .: "value"
        <&> fromDegreeCelsiusAbsolute
    _ -> typeMismatch "expected unit C" (Object o)

instance FromJSON WeatherForecast where
  parseJSON = withObject "WeatherForecast" $ \o -> do
    forecastDate <-
      o .: "forecastDate" >>= parseTimeM @_ @Day False defaultTimeLocale "%Y%m%d" . fromMisoString
    week <- o .: "week"
    forecastWind <- o .: "forecastWind"
    forecastWeather <- o .: "forecastWeather"
    forecastIcon <- o .: "ForecastIcon"
    forecastTemp <- do
      min' <- o .: "forecastMintemp" >>= parseJSON_DegreeCelsius
      max' <- o .: "forecastMaxtemp" >>= parseJSON_DegreeCelsius
      pure $ Finite min' <=..<= Finite max'
    forecastRH <- do
      let parsePercent = \case
            "percent" -> Right percent
            _ -> Left "percent as unit"
      min' <- o .: "forecastMinrh" >>= parseJSON_Unit parsePercent
      max' <- o .: "forecastMaxrh" >>= parseJSON_Unit parsePercent
      pure $ Finite min' <=..<= Finite max'
    psr <-
      o .: "PSR" >>= \case
        txt
          | txt `elem` ["High", "Medium High", "Medium", "Medium Low", "Low"] -> pure $ fromMisoString txt -- TEMP FIXME
          | otherwise -> typeMismatch "PSR" $ String txt
    pure $
      WeatherForecast
        forecastDate
        week
        forecastWind
        forecastWeather
        forecastTemp
        forecastRH
        psr
        forecastIcon

data NineDayWeatherForecast = NineDayWeatherForecast
  { weatherForecast :: [WeatherForecast], -- Weather Forecast
    soilTemp :: [SoilTemp], -- Soil Temperature, NOTE: doc didn't say it is a list, but you know...
    seaTemp :: SeaTemp, -- Sea Surface Temperature
    -----------------------------------------------------------------
    -- NOTE: Below are not in doc but exists in response. Nice doc --
    -----------------------------------------------------------------
    generalSituation :: MisoString,
    updateTime :: TimeData
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSVal, FromJSVal)

data DataWithInterval a = DataWithInterval
  { --  Start Time YYYY-MM-DD'T'hh:mm:ssZ Example: 2020-09-01T08:19:00+08:00 endTime End Time
    --  End Time YYYY-MM-DD'T'hh:mm:ssZ Example: 2020-09-01T08:19:00+08:00 endTime End Time
    interval :: Interval TimeData,
    _data :: [a]
  }
  deriving stock (Eq, Show, Generic)

instance (FromJSON a) => FromJSON (DataWithInterval a) where
  parseJSON = withObject "DataWithInterval" $ \o -> do
    t1 <- Finite <$> o .: "startTime"
    t2 <- Finite <$> o .: "endTime"
    DataWithInterval (t1 <=..< t2) <$> o .: "data"

deriving anyclass instance (ToJSVal a) => ToJSVal (DataWithInterval a)

deriving anyclass instance (FromJSVal a) => FromJSVal (DataWithInterval a)

data Lightning = Lightning
  { place :: MisoString, -- location
    occur :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSVal, FromJSVal)

data Rainfall = Rainfall
  { interval :: Interval (Length Scientific), -- Minimum & Maximum rainfall record
    place :: MisoString, -- location
    main :: Bool -- Maintenance flag (TRUE/FALSE)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSVal, FromJSVal)

instance FromJSON Rainfall where
  parseJSON = withObject "Rainfall" $ \o -> do
    place <- o .: "place"
    main' <-
      o .: "main"
        >>= \case
          "TRUE" -> pure True
          "FALSE" -> pure False
          txt -> typeMismatch "main" $ String txt
    unit <-
      o .: "unit" >>= \case
        "mm" -> pure $ milli meter
        txt -> typeMismatch "mm as unit" $ String txt
    min'' <-
      o .:? "min" <&> \case
        Just min' -> Finite $ min' *~ unit
        Nothing -> Finite $ 0 *~ unit
    max'' <-
      o .:? "max" <&> \case
        Just max' -> Finite $ max' *~ unit
        Nothing -> Finite $ 0 *~ unit
    pure $ Rainfall (min'' <=..<= max'') place main'

data UVIndexData = UVIndexData
  { place :: MisoString, -- location
    value :: Scientific, -- value
    desc :: MisoString, -- description
    message :: Maybe MisoString -- message
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSVal, FromJSVal)

instance FromJSON UVIndexData where
  parseJSON = withObject "UVIndexData" $ \o ->
    UVIndexData
      <$> o .: "place"
      <*> o .: "value"
      <*> o .: "desc"
      <*> o .:? "message"

data UVIndex = UVIndex (NonEmpty UVIndexData) | NoUVIndexData
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSVal, FromJSVal)

instance FromJSON UVIndex where
  parseJSON v =
    ( withObject "UVIndex" $ \o -> do
        o .: "recordDesc" >>= \case
          "During the past hour" -> UVIndex <$> o .: "data"
          -- NOTE: Seems like the text is always "During the past hour"
          txt -> typeMismatch "string (During the past hour)" $ String txt
    )
      v
      <|> ( withText "UVIndex" $
              -- NOTE: it just return empty string at night!! not mentioned in doc, so nice!
              \case
                "" -> pure NoUVIndexData
                t -> typeMismatch "empty string" $ String t
          )
        v

data DataWithRecordTime a = DataWithRecordTime
  { recordTime :: TimeData, -- record time YYYY-MMDD'T'hh:mm:ssZ Example: 2020-09- 01T08:19:00+08:00
    _data :: [a]
  }
  deriving stock (Show, Eq, Generic)

deriving anyclass instance (ToJSVal a) => (ToJSVal (DataWithRecordTime a))

deriving anyclass instance (FromJSVal a) => (FromJSVal (DataWithRecordTime a))

instance (FromJSON a) => FromJSON (DataWithRecordTime a) where
  parseJSON = withObject "DataWithRecordTime" $ \o ->
    DataWithRecordTime <$> o .: "recordTime" <*> o .: "data"

data Temperature = Temperature
  { place :: MisoString, -- location
    value :: ThermodynamicTemperature Scientific -- value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSVal, FromJSVal)

instance FromJSON Temperature where
  parseJSON = withObject "Temperature" $ \o -> Temperature <$> o .: "place" <*> parseJSON_DegreeCelsius (Object o)

data Humidity = Humidity
  { place :: MisoString, -- location
    value :: Dimensionless Scientific -- value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSVal, FromJSVal)

instance FromJSON Humidity where
  parseJSON = withObject "Humidity" $ \o -> do
    place <- o .: "place"
    value <-
      parseJSON_Unit
        ( \case
            "percent" -> Right percent
            _ -> Left "percent as unit"
        )
        $ Object o
    pure $ Humidity place value

data CurrentWeatherReport = CurrentWeatherReport
  { lightning :: Maybe (DataWithInterval Lightning),
    rainfall :: DataWithInterval Rainfall,
    icon :: [Natural], -- Icon Return a List Weather icon list: https://www.hko.gov.hk/textonly/v2/expla in/wxicon_e.htm
    iconUpdateTime :: TimeData, -- Icon Update Time YYYY-MM-DD'T'hh:mm:ssZ Example: 2020-09-01T08:19:00+08:00
    uvindex :: UVIndex,
    updateTime :: TimeData, -- Update Time YYYY-MM-DD'T'hh:mm:ssZ Example: 2020-09-01T08:19:00+08:00
    warningMessage :: [MisoString], -- Warning Message Return a List. If no data for warning message, empty string will be returned.
    rainstormReminder :: Maybe MisoString, -- Rainstorm Reminder
    specialWxTips :: [MisoString], -- Special Weather Tips
    tcmessage :: [MisoString], -- Message of tropical cyclone position Return a List. NOTE: got empty text, are you kidding me?
    mintempFrom00To09 :: Maybe MisoString, -- Minimum temperature from midnight to 9 am
    rainfallFrom00To12 :: Maybe MisoString, -- Accumulated rainfall at HKO from midnight to noon
    rainfallLastMonth :: Maybe MisoString, -- Rainfall in last month
    rainfallJanuaryToLastMonth :: Maybe MisoString, --  Accumulated rainfall from January to last month
    temperature :: DataWithRecordTime Temperature, -- Temperature
    humidity :: DataWithRecordTime Humidity -- Humidity
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSVal, FromJSVal)

instance FromJSON CurrentWeatherReport where
  parseJSON = withObject "CurrentWeatherReport" $ \o -> do
    lightning <- o .:? "lightning"
    rainfall <- o .: "rainfall"
    icon <- o .: "icon"
    iconUpateTime <- o .: "iconUpdateTime"
    uvIndex <- o .: "uvindex"
    updateTime <- o .: "updateTime"
    warningMessage <-
      o .: "warningMessage"
        <|> ( o .: "warningMessage" >>= \case
                ("" :: MisoString) -> pure []
                _ -> typeMismatch "warningMessage" (Object o)
            )
    rainstormReminder <- o .:? "rainstormReminder"
    specialWxTips <- o .:? "specialWxTips" .!= []
    tcmessage <-
      o .: "tcmessage"
        <|> ( o .: "tcmessage" >>= \case
                ("" :: MisoString) -> pure []
                _ -> typeMismatch "tcmessage" (Object o)
            )
    mintempFrom00To09 <- o .:? "mintempFrom00To09"
    rainfallFrom00To12 <- o .:? "rainfallFrom00To12"
    rainfallLastMonth <- o .:? "rainfallLastMonth"
    rainfallJanuaryToLastMonth <- o .:? "rainfallJanuaryToLastMonth"
    temperature <- o .: "temperature"
    humidity <- o .: "humidity"
    pure $
      CurrentWeatherReport
        lightning
        rainfall
        icon
        iconUpateTime
        uvIndex
        updateTime
        warningMessage
        rainstormReminder
        specialWxTips
        tcmessage
        mintempFrom00To09
        rainfallFrom00To12
        rainfallLastMonth
        rainfallJanuaryToLastMonth
        temperature
        humidity
