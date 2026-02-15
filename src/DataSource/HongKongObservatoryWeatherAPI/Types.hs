{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module DataSource.HongKongObservatoryWeatherAPI.Types where

import Control.Applicative
import Data.Functor
import Data.Interval
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
import Utils.JSON

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
  deriving anyclass (FromJSON, ToJSVal)

data SoilTemp = SoilTemp
  { place :: MisoString, -- location
    value :: ThermodynamicTemperature Scientific, -- value
    recordTime :: TimeData, -- record time YYYY-MMDD'T'hh:mm:ssZ Example: 2020-09- 01T08:19:00+08:00
    depth :: Length Scientific
  }
  deriving stock (Eq, Show)

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

instance ToJSVal SoilTemp where
  toJSVal (SoilTemp place value recordTime depth) = do
    o <- create
    setProp "place" place o
    setProp "value" (toDegreeCelsiusAbsolute value) o
    setProp @MisoString "unit" "C" o
    setProp "recordTime" recordTime o
    o_depth <- create
    setProp "value" (depth /~ meter) o_depth
    setProp @MisoString "unit" "meter" o_depth
    setProp "depth" o_depth o
    toJSVal o

data SeaTemp = SeaTemp
  { place :: MisoString, -- location
    value :: ThermodynamicTemperature Scientific, -- value
    recordTime :: TimeData -- record time YYYY-MMDD'T'hh:mm:ssZ Example: 2020-09- 01T08:19:00+08:00
  }
  deriving stock (Eq, Show)

instance FromJSON SeaTemp where
  parseJSON = withObject "SeaTemp" $ \o ->
    SeaTemp
      <$> o .: "place"
      <*> parseJSON_DegreeCelsius (Object o)
      <*> o .: "recordTime"

instance ToJSVal SeaTemp where
  toJSVal (SeaTemp place value recordTime) = do
    o <- create
    setProp "place" place o
    setProp "value" (toDegreeCelsiusAbsolute value) o
    setProp @MisoString "unit" "C" o
    setProp "recordTime" recordTime o
    toJSVal o

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
  deriving stock (Eq, Show)

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

instance ToJSVal WeatherForecast where
  toJSVal (WeatherForecast {..}) = do
    o <- create
    setProp "forecastDate" (formatTime defaultTimeLocale "%Y%m%d" forecastDate) o
    setProp "week" week o
    setProp "forecastWind" forecastWind o
    setProp "forecastWeather" forecastWeather o
    setProp "ForecastIcon" forecastIcon o
    case (lowerBound' forecastTempInterval, upperBound' forecastTempInterval) of
      ((Finite lb, Closed), (Finite ub, Closed)) -> do
        o_forecastMintemp <- create
        setProp "value" (toDegreeCelsiusAbsolute lb) o_forecastMintemp
        setProp @MisoString "unit" "C" o_forecastMintemp
        setProp "forecastMintemp" o_forecastMintemp o
        o_forecastMaxtemp <- create
        setProp "value" (toDegreeCelsiusAbsolute ub) o_forecastMaxtemp
        setProp @MisoString "unit" "C" o_forecastMaxtemp
        setProp "forecastMaxtemp" o_forecastMaxtemp o
      _ -> error "toJSVal WeatherForecast: unexpected forecast Temp interval"
    case (lowerBound' forecastRHInterval, upperBound' forecastRHInterval) of
      ((Finite lb, Closed), (Finite ub, Closed)) -> do
        o_forecastMinRH <- create
        setProp "value" (lb /~ percent) o_forecastMinRH
        setProp @MisoString "unit" "percent" o_forecastMinRH
        setProp "forecastMinrh" o_forecastMinRH o
        o_forecastMaxRH <- create
        setProp "value" (ub /~ percent) o_forecastMaxRH
        setProp @MisoString "unit" "percent" o_forecastMaxRH
        setProp "forecastMaxrh" o_forecastMaxRH o
      _ -> error "toJSVal WeatherForecast: unexpected forecast rh interval"
    setProp "PSR" psr o
    toJSVal o

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
  deriving anyclass (FromJSON, ToJSVal)

data DataWithInterval a = DataWithInterval
  { --  Start Time YYYY-MM-DD'T'hh:mm:ssZ Example: 2020-09-01T08:19:00+08:00 endTime End Time
    --  End Time YYYY-MM-DD'T'hh:mm:ssZ Example: 2020-09-01T08:19:00+08:00 endTime End Time
    interval :: Interval TimeData,
    _data :: [a]
  }
  deriving stock (Eq, Show)

instance (FromJSON a) => FromJSON (DataWithInterval a) where
  parseJSON = withObject "DataWithInterval" $ \o -> do
    t1 <- Finite <$> o .: "startTime"
    t2 <- Finite <$> o .: "endTime"
    DataWithInterval (t1 <=..< t2) <$> o .: "data"

instance (ToJSVal a) => ToJSVal (DataWithInterval a) where
  toJSVal (DataWithInterval interval' d) = do
    o <- create
    case (lowerBound' interval', upperBound' interval') of
      ((Finite lb, Closed), (Finite ub, Open)) -> do
        setProp "startTime" lb o
        setProp "endTime" ub o
        setProp "data" d o
        toJSVal o
      _ -> error "unexpected DataWithInterval toJSON failed"

data Lightning = Lightning
  { place :: MisoString, -- location
    occur :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSVal)

data Rainfall = Rainfall
  { interval :: Interval (Length Scientific), -- Minimum & Maximum rainfall record
    place :: MisoString, -- location
    main :: Bool -- Maintenance flag (TRUE/FALSE)
  }
  deriving stock (Eq, Show)

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
        Nothing -> NegInf
    max'' <-
      o .:? "max" <&> \case
        Just max' -> Finite $ max' *~ unit
        Nothing -> PosInf
    pure $ Rainfall (min'' <=..<= max'') place main'

instance ToJSVal Rainfall where
  toJSVal (Rainfall interval' place main) = case (lowerBound' interval', upperBound' interval') of
    ((lb, Closed), (ub, Closed)) -> do
      o <- create
      setProp "place" place o
      setProp "main" main o
      setProp @MisoString "unit" "mm" o
      case lb of
        Finite lb' -> setProp "min" (lb' /~ milli meter) o
        NegInf -> pure ()
        PosInf -> error "toJSVal: unexpected PosInf lower bound interval in Rainfall"
      case ub of
        Finite ub' -> setProp "max" (ub' /~ milli meter) o
        NegInf -> error "toJSVal: unexpected NegInf upper bound interval in Rainfall"
        PosInf -> pure ()
      toJSVal o
    _ -> error "toJSVal: unexpected interval in Rainfall"

data UVIndexData = UVIndexData
  { place :: MisoString, -- location
    value :: Scientific, -- value
    desc :: MisoString, -- description
    message :: Maybe MisoString -- message
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSVal)

data UVIndex = UVIndex
  { _data :: [UVIndexData],
    recordDesc :: MisoString -- record description
  }
  deriving stock (Show, Eq)

instance FromJSON UVIndex where
  parseJSON v =
    ( withText "UVIndex" $
        -- NOTE: it just return empty string at night!! not mentioned in doc, so nice!
        \case
          "" -> pure $ UVIndex [] "nighttime no uv!"
          t -> typeMismatch "empty string" $ String t
    )
      v
      <|> (withObject "UVIndex" $ \o -> UVIndex <$> o .: "data" <*> o .: "recordDesc") v

instance ToJSVal UVIndex where
  toJSVal (UVIndex _d recordDesc) = do
    o <- create
    setProp "data" _d o
    setProp "recordDesc" recordDesc o
    toJSVal o

data DataWithRecordTime a = DataWithRecordTime
  { recordTime :: TimeData, -- record time YYYY-MMDD'T'hh:mm:ssZ Example: 2020-09- 01T08:19:00+08:00
    _data :: [a]
  }
  deriving stock (Eq, Show)

instance (FromJSON a) => FromJSON (DataWithRecordTime a) where
  parseJSON = withObject "DataWithRecordTime" $ \o ->
    DataWithRecordTime <$> o .: "recordTime" <*> o .: "data"

instance (ToJSVal a) => ToJSVal (DataWithRecordTime a) where
  toJSVal (DataWithRecordTime recordTime _d) = do
    o <- create
    setProp "recordTime" recordTime o
    setProp "data" _d o
    toJSVal o

data Temperature = Temperature
  { place :: MisoString, -- location
    value :: ThermodynamicTemperature Scientific -- value
  }
  deriving stock (Eq, Show)

instance FromJSON Temperature where
  parseJSON = withObject "Temperature" $ \o -> Temperature <$> o .: "place" <*> parseJSON_DegreeCelsius (Object o)

instance ToJSVal Temperature where
  toJSVal (Temperature place value) = do
    o <- create
    setProp "place" place o
    setProp "value" (toDegreeCelsiusAbsolute value) o
    setProp @MisoString "unit" "C" o
    toJSVal o

data Humidity = Humidity
  { place :: MisoString, -- location
    value :: Dimensionless Scientific -- value
  }
  deriving stock (Eq, Show)

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

instance ToJSVal Humidity where
  toJSVal (Humidity place value) = do
    o <- create
    setProp "place" place o
    setProp "value" (value /~ percent) o
    setProp @MisoString "unit" "percent" o
    toJSVal o

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
  deriving stock (Eq, Show)

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
    specialWxTips <- o .: "specialWxTips" .!= []
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

instance ToJSVal CurrentWeatherReport where
  toJSVal (CurrentWeatherReport {..}) = do
    o <- create
    setProp "lightning" lightning o
    setProp "rainfall" rainfall o
    setProp "icon" icon o
    setProp "iconUpdateTime" iconUpdateTime o
    setProp "uvindex" uvindex o
    setProp "updateTime" updateTime o
    setProp "warningMessage" warningMessage o
    setProp "rainstormReminder" rainstormReminder o
    setProp "specialWxTips" specialWxTips o
    setProp "tcmessage" tcmessage o
    setProp "mintempFrom00To09" mintempFrom00To09 o
    setProp "rainfallFrom00To12" rainfallFrom00To12 o
    setProp "rainfallLastMonth" rainfallLastMonth o
    setProp "rainfallJanuaryToLastMonth" rainfallJanuaryToLastMonth o
    setProp "temperature" temperature o
    setProp "humidity" humidity o
    toJSVal o
