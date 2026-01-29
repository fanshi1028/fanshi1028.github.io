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
import Data.Text hiding (concat, elem, foldl', foldr, reverse, show)
import Data.Time
import GHC.Generics
import Miso.DSL
import Miso.String (MisoString)
import Numeric.Natural
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.SIUnits hiding (fromDegreeCelsiusAbsolute, toDegreeCelsiusAbsolute)
import Utils.Dimensional
import Utils.JSON

data LocalWeatherForecast = LocalWeatherForecast
  { generalSituation :: StrictText, -- General Situation
    tcInfo :: StrictText, -- Tropical Cyclone Information
    fireDangerWarning :: StrictText, -- Fire Danger Warning Message
    forecastPeriod :: StrictText, -- Forecast Period
    forecastDesc :: StrictText, -- Forecast Description
    outlook :: StrictText, -- Outlook
    updateTime :: UTCTime -- Update Time YYYY-MM-DD'T'hh:mm:ssZ Example: 2020-09-01T08:19:00+08:00
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSVal, ToJSVal)

data SoilTemp = SoilTemp
  { place :: StrictText, -- location
    value :: ThermodynamicTemperature Scientific, -- value
    recordTime :: UTCTime, -- record time YYYY-MMDD'T'hh:mm:ssZ Example: 2020-09- 01T08:19:00+08:00
    depth :: Length Scientific
  }
  deriving stock (Eq, Show)

instance FromJSVal SoilTemp where
  fromJSVal o = do
    mPlace <- o ! "place" >>= fromJSVal
    mTemp <- parseJSVal_DegreeCelsius o
    mRecordTime <- o ! "recordTime" >>= fromJSVal
    mDepth <-
      o ! "depth"
        >>= fromJSVal_Unit
          ( \case
              "metre" -> Just meter
              _ -> Nothing
          )
    pure $ SoilTemp <$> mPlace <*> mTemp <*> mRecordTime <*> mDepth

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
  { place :: StrictText, -- location
    value :: ThermodynamicTemperature Scientific, -- value
    recordTime :: UTCTime -- record time YYYY-MMDD'T'hh:mm:ssZ Example: 2020-09- 01T08:19:00+08:00
  }
  deriving stock (Eq, Show)

instance FromJSVal SeaTemp where
  fromJSVal o = do
    mPlace <- o ! "place" >>= fromJSVal
    mTemp <- parseJSVal_DegreeCelsius o
    mRecordTime <- o ! "recordTime" >>= fromJSVal
    pure $ SeaTemp <$> mPlace <*> mTemp <*> mRecordTime

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
    forecastWind :: StrictText, -- Forecast Wind
    forecastWeather :: StrictText, -- Forecast Weather
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
    psr :: StrictText, -- TEMP FIXME
    forecastIcon :: Natural
  }
  deriving stock (Eq, Show)

fromJSVal_Unit :: (FromJSVal a, Num a) => (StrictText -> (Maybe (Unit m d a))) -> JSVal -> IO (Maybe (Quantity d a))
fromJSVal_Unit parseUnit v = do
  mValue <- v ! "value" >>= fromJSVal
  mUnit <- v ! "unit" >>= fromJSVal <&> (>>= parseUnit)
  pure $ (*~) <$> mValue <*> mUnit

parseJSVal_DegreeCelsius :: (FromJSVal a, Fractional a) => JSVal -> IO (Maybe (ThermodynamicTemperature a))
parseJSVal_DegreeCelsius v =
  v ! "unit" >>= fromJSVal @StrictText >>= \case
    Just "C" ->
      v ! "value"
        >>= fromJSVal
        <&> fmap fromDegreeCelsiusAbsolute
    _ -> pure Nothing

instance FromJSVal WeatherForecast where
  fromJSVal o = do
    mForecastDate <-
      (>>= parseTimeM @_ @Day False defaultTimeLocale "%Y%m%d")
        <$> (o ! "forecastDate" >>= fromJSVal)
    mWeek <- o ! "week" >>= fromJSVal
    mForecastWind <- o ! "forecastWind" >>= fromJSVal
    mForecastWeather <- o ! "forecastWeather" >>= fromJSVal
    mForecastIcon <- o ! "ForecastIcon" >>= fromJSVal
    mForecastTemp <- do
      mMin <- o ! "forecastMintemp" >>= parseJSVal_DegreeCelsius
      mMax <- o ! "forecastMaxtemp" >>= parseJSVal_DegreeCelsius
      pure $ (<=..<=) <$> (Finite <$> mMin) <*> (Finite <$> mMax)
    mForecastRH <- do
      let parsePercent = \case
            "percent" -> Just percent
            _ -> Nothing
      mMin <- o ! "forecastMinrh" >>= fromJSVal_Unit parsePercent
      mMax <- o ! "forecastMaxrh" >>= fromJSVal_Unit parsePercent
      pure $ (<=..<=) <$> (Finite <$> mMin) <*> (Finite <$> mMax)
    mPSR <-
      o ! "PSR" >>= fromJSVal <&> \case
        Just txt
          | txt `elem` ["High", "Medium High", "Medium", "Medium Low", "Low"] -> Just txt -- TEMP FIXME
          | otherwise -> Nothing
        _ -> Nothing
    pure $
      WeatherForecast
        <$> mForecastDate
        <*> mWeek
        <*> mForecastWind
        <*> mForecastWeather
        <*> mForecastTemp
        <*> mForecastRH
        <*> mPSR
        <*> mForecastIcon

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
    generalSituation :: StrictText,
    updateTime :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSVal, ToJSVal)

data DataWithInterval a = DataWithInterval
  { --  Start Time YYYY-MM-DD'T'hh:mm:ssZ Example: 2020-09-01T08:19:00+08:00 endTime End Time
    --  End Time YYYY-MM-DD'T'hh:mm:ssZ Example: 2020-09-01T08:19:00+08:00 endTime End Time
    interval :: Interval UTCTime,
    _data :: [a]
  }
  deriving stock (Eq, Show)

instance (FromJSVal a) => FromJSVal (DataWithInterval a) where
  fromJSVal o = do
    mT1 <- (fmap Finite) <$> (o ! "startTime" >>= fromJSVal)
    mT2 <- (fmap Finite) <$> (o ! "endTime" >>= fromJSVal)
    let mInterval = (<=..<) <$> mT1 <*> mT2
    mData <- o ! "data" >>= fromJSVal
    pure $ DataWithInterval <$> mInterval <*> mData

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
  { place :: StrictText, -- location
    occur :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSVal, ToJSVal)

data Rainfall = Rainfall
  { interval :: Interval (Length Scientific), -- Minimum & Maximum rainfall record
    place :: StrictText, -- location
    main :: Bool -- Maintenance flag (TRUE/FALSE)
  }
  deriving stock (Eq, Show)

instance FromJSVal Rainfall where
  fromJSVal o = do
    mPlace <- o ! "place" >>= fromJSVal
    mMain <-
      o ! "main"
        >>= fromJSVal @MisoString
        <&> ( >>=
                \case
                  "TRUE" -> Just True
                  "FALSE" -> Just False
                  _ -> Nothing
            )
    mUnit <-
      o ! "unit"
        >>= fromJSVal @MisoString
        <&> ( >>=
                \case
                  "mm" -> Just $ milli meter
                  _ -> Nothing
            )
    mMin <-
      o ! "min" >>= fromJSVal <&> \case
        Just (Just min') -> Finite . (min' *~) <$> mUnit
        Just Nothing -> Just NegInf
        Nothing -> Nothing
    mMax <-
      o ! "max" >>= fromJSVal <&> \case
        Just (Just max') -> Finite . (max' *~) <$> mUnit
        Just Nothing -> Just PosInf
        Nothing -> Nothing
    pure $
      Rainfall
        <$> ((<=..<=) <$> mMin <*> mMax)
        <*> mPlace
        <*> mMain

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
  { place :: StrictText, -- location
    value :: Scientific, -- value
    desc :: StrictText, -- description
    message :: Maybe StrictText -- message
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSVal, ToJSVal)

data UVIndex = UVIndex
  { _data :: [UVIndexData],
    recordDesc :: StrictText -- record description
  }
  deriving stock (Show, Eq)

instance FromJSVal UVIndex where
  fromJSVal o =
    (<|>)
      <$>
      -- NOTE: it just return empty string at night!! not mentioned in doc, so nice!
      ( fromJSVal @MisoString o <&> \case
          Just "" -> Just $ UVIndex [] "nighttime no uv!"
          _ -> Nothing
      )
      <*> do
        mData <- o ! "data" >>= fromJSVal
        mRecordDesc <- o ! "recordDesc" >>= fromJSVal
        pure $ UVIndex <$> mData <*> mRecordDesc

instance ToJSVal UVIndex where
  toJSVal (UVIndex _d recordDesc) = do
    o <- create
    setProp "data" _d o
    setProp "recordDesc" recordDesc o
    toJSVal o

data DataWithRecordTime a = DataWithRecordTime
  { recordTime :: UTCTime, -- record time YYYY-MMDD'T'hh:mm:ssZ Example: 2020-09- 01T08:19:00+08:00
    _data :: [a]
  }
  deriving stock (Eq, Show)

instance (FromJSVal a) => FromJSVal (DataWithRecordTime a) where
  fromJSVal o = do
    mRecordTime <- o ! "recordTime" >>= fromJSVal
    mData <- o ! "data" >>= fromJSVal
    pure $ DataWithRecordTime <$> mRecordTime <*> mData

instance (ToJSVal a) => ToJSVal (DataWithRecordTime a) where
  toJSVal (DataWithRecordTime recordTime _d) = do
    o <- create
    setProp "recordTime" recordTime o
    setProp "data" _d o
    toJSVal o

data Temperature = Temperature
  { place :: StrictText, -- location
    value :: ThermodynamicTemperature Scientific -- value
  }
  deriving stock (Eq, Show)

instance FromJSVal Temperature where
  fromJSVal o = do
    mPlace <- o ! "place" >>= fromJSVal
    mValue <- parseJSVal_DegreeCelsius o
    pure $ Temperature <$> mPlace <*> mValue

instance ToJSVal Temperature where
  toJSVal (Temperature place value) = do
    o <- create
    setProp "place" place o
    setProp "value" (toDegreeCelsiusAbsolute value) o
    setProp @MisoString "unit" "C" o
    toJSVal o

data Humidity = Humidity
  { place :: StrictText, -- location
    value :: Dimensionless Scientific -- value
  }
  deriving stock (Eq, Show)

instance FromJSVal Humidity where
  fromJSVal o = do
    mPlace <- o ! "place" >>= fromJSVal
    mValue <-
      fromJSVal_Unit
        ( \case
            "percent" -> Just percent
            _ -> Nothing
        )
        o
    pure $ Humidity <$> mPlace <*> mValue

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
    iconUpdateTime :: UTCTime, -- Icon Update Time YYYY-MM-DD'T'hh:mm:ssZ Example: 2020-09-01T08:19:00+08:00
    uvindex :: UVIndex,
    updateTime :: UTCTime, -- Update Time YYYY-MM-DD'T'hh:mm:ssZ Example: 2020-09-01T08:19:00+08:00
    warningMessage :: [StrictText], -- Warning Message Return a List. If no data for warning message, empty string will be returned.
    rainstormReminder :: Maybe StrictText, -- Rainstorm Reminder
    specialWxTips :: [StrictText], -- Special Weather Tips
    tcmessage :: [StrictText], -- Message of tropical cyclone position Return a List. NOTE: got empty text, are you kidding me?
    mintempFrom00To09 :: Maybe StrictText, -- Minimum temperature from midnight to 9 am
    rainfallFrom00To12 :: Maybe StrictText, -- Accumulated rainfall at HKO from midnight to noon
    rainfallLastMonth :: Maybe StrictText, -- Rainfall in last month
    rainfallJanuaryToLastMonth :: Maybe StrictText, --  Accumulated rainfall from January to last month
    temperature :: DataWithRecordTime Temperature, -- Temperature
    humidity :: DataWithRecordTime Humidity -- Humidity
  }
  deriving stock (Eq, Show)

instance FromJSVal CurrentWeatherReport where
  fromJSVal o = do
    mLightning <- o ! "lightning" >>= fromJSVal
    mRainfall <- o ! "rainfall" >>= fromJSVal
    mIcon <- o ! "icon" >>= fromJSVal
    mIconUpateTime <- o ! "iconUpdateTime" >>= fromJSVal
    mUVIndex <- o ! "uvindex" >>= fromJSVal
    mUpateTime <- o ! "updateTime" >>= fromJSVal
    mWarningMessage <- do
      o' <- o ! "warningMessage"
      (<|>)
        <$> fromJSVal o'
        <*> ( fromJSVal @StrictText o' <&> \case
                Just "" -> Just []
                _ -> Nothing
            )
    mRainstormReminder <- o ! "rainstormReminder" >>= fromJSVal
    mSpecialWxTips <-
      o ! "specialWxTips" >>= \o' ->
        isUndefined o' >>= \case
          True -> pure $ Just []
          False -> fromJSVal o'
    mTcmessage <- do
      o' <- o ! "tcmessage"
      (<|>)
        <$> fromJSVal o'
        <*> ( fromJSVal @StrictText o' <&> \case
                Just "" -> Just []
                _ -> Nothing
            )
    mMintempFrom00To09 <- o ! "mintempFrom00To09" >>= fromJSVal
    mRainfallFrom00To12 <- o ! "rainfallFrom00To12" >>= fromJSVal
    mRainfallLastMonth <- o ! "rainfallLastMonth" >>= fromJSVal
    mRainfallJanuaryToLastMonth <- o ! "rainfallJanuaryToLastMonth" >>= fromJSVal
    mTemperature <- o ! "temperature" >>= fromJSVal
    mHumidity <- o ! "humidity" >>= fromJSVal
    pure $
      CurrentWeatherReport
        <$> mLightning
        <*> mRainfall
        <*> mIcon
        <*> mIconUpateTime
        <*> mUVIndex
        <*> mUpateTime
        <*> mWarningMessage
        <*> mRainstormReminder
        <*> mSpecialWxTips
        <*> mTcmessage
        <*> mMintempFrom00To09
        <*> mRainfallFrom00To12
        <*> mRainfallLastMonth
        <*> mRainfallJanuaryToLastMonth
        <*> mTemperature
        <*> mHumidity

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
