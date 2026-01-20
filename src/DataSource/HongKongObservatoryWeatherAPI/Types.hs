{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module DataSource.HongKongObservatoryWeatherAPI.Types where

import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Control.Applicative
import Data.Functor
import Data.Interval
import Data.Scientific
import Data.Text hiding (concat, elem, foldl', foldr, reverse, show)
import Data.Time
import GHC.Generics
import Miso.DSL
import Numeric.Natural
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.SIUnits hiding (fromDegreeCelsiusAbsolute)
import Utils.Dimensional
import Utils.JSON
import Utils.Serialise

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
  deriving anyclass (Serialise, FromJSVal)

data SoilTemp = SoilTemp
  { place :: StrictText, -- location
    value :: ThermodynamicTemperature Scientific, -- value
    recordTime :: UTCTime, -- record time YYYY-MMDD'T'hh:mm:ssZ Example: 2020-09- 01T08:19:00+08:00
    depth :: Length Scientific
  }
  deriving stock (Eq, Show)

instance Serialise SoilTemp where
  encode (SoilTemp place value recordTime depth) = encodeListLen 5 <> encodeWord 0 <> encode place <> encodeScientific (value /~ degreeCelsius) <> encode recordTime <> encodeScientific (depth /~ meter)
  decode =
    (,) <$> decodeListLen <*> decodeWord >>= \case
      (5, 0) -> SoilTemp <$> decode <*> ((*~ degreeCelsius) <$> decodeScientific) <*> decode <*> ((*~ meter) <$> decodeScientific)
      _ -> fail "invalid SoilTemp encoding"

instance FromJSVal SoilTemp where
  fromJSVal o = do
    mPlace <- o ! "place" >>= fromJSVal
    mTemp <- parseJSVal_DegreeCelsius o
    mRecordTime <- o ! "recordTime" >>= fromJSVal
    mDepth <-
      o ! "depth"
        >>= parseJSVal_Unit
          ( \case
              "metre" -> Just meter
              _ -> Nothing
          )
    pure $ SoilTemp <$> mPlace <*> mTemp <*> mRecordTime <*> mDepth

data SeaTemp = SeaTemp
  { place :: StrictText, -- location
    value :: ThermodynamicTemperature Scientific, -- value
    recordTime :: UTCTime -- record time YYYY-MMDD'T'hh:mm:ssZ Example: 2020-09- 01T08:19:00+08:00
  }
  deriving stock (Eq, Show)

instance Serialise SeaTemp where
  encode (SeaTemp place value recordTime) = encodeListLen 4 <> encodeWord 0 <> encode place <> encodeScientific (value /~ degreeCelsius) <> encode recordTime
  decode =
    (,) <$> decodeListLen <*> decodeWord >>= \case
      (4, 0) -> SeaTemp <$> decode <*> ((*~ degreeCelsius) <$> decodeScientific) <*> decode
      _ -> fail "invalid SoilTemp encoding"

instance FromJSVal SeaTemp where
  fromJSVal o = do
    mPlace <- o ! "place" >>= fromJSVal
    mTemp <- parseJSVal_DegreeCelsius o
    mRecordTime <- o ! "recordTime" >>= fromJSVal
    pure $ SeaTemp <$> mPlace <*> mTemp <*> mRecordTime

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

instance Serialise WeatherForecast where
  encode (WeatherForecast forcastDate week forecastWind forecastWeather forecastTempInterval forecastRHInterval psr forecastIcon) =
    encodeListLen 9
      <> encodeWord 0
      <> encode (toModifiedJulianDay forcastDate)
      <> encode (fromEnum week)
      <> encode forecastWind
      <> encode forecastWeather
      <> encodeInterval (encodeScientific . (/~ degreeCelsius)) forecastTempInterval
      <> encodeInterval (encodeScientific . (/~ percent)) forecastRHInterval
      <> encode psr
      <> encode forecastIcon
  decode = do
    (,) <$> decodeListLen <*> decodeWord >>= \case
      (9, 0) ->
        WeatherForecast
          <$> (ModifiedJulianDay <$> decode)
          <*> (toEnum <$> decode)
          <*> decode
          <*> decode
          <*> decodeInterval ((*~ degreeCelsius) <$> decodeScientific)
          <*> decodeInterval ((*~ percent) <$> decodeScientific)
          <*> decode
          <*> decode
      _ -> fail "invalid WeatherForecast encoding"

parseJSVal_Unit :: (FromJSVal a, Num a) => (StrictText -> (Maybe (Unit m d a))) -> JSVal -> IO (Maybe (Quantity d a))
parseJSVal_Unit parseUnit v = do
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
      mMin <- o ! "forecastMinrh" >>= parseJSVal_Unit parsePercent
      mMax <- o ! "forecastMaxrh" >>= parseJSVal_Unit parsePercent
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
  deriving anyclass (Serialise, FromJSVal)

data DataWithInterval a = DataWithInterval
  { --  Start Time YYYY-MM-DD'T'hh:mm:ssZ Example: 2020-09-01T08:19:00+08:00 endTime End Time
    --  End Time YYYY-MM-DD'T'hh:mm:ssZ Example: 2020-09-01T08:19:00+08:00 endTime End Time
    interval :: Interval UTCTime,
    _data :: [a]
  }
  deriving stock (Eq, Show)

encodeDataWithInterval :: (a -> Encoding) -> DataWithInterval a -> Encoding
encodeDataWithInterval encoder (DataWithInterval interval' _data) = encodeListLen 3 <> encodeWord 0 <> encodeInterval encode interval' <> encodeListWith encoder _data

decodeDataWithInterval :: Decoder s a -> Decoder s (DataWithInterval a)
decodeDataWithInterval decoder =
  (,) <$> decodeListLen <*> decodeWord >>= \case
    (3, 0) -> DataWithInterval <$> decodeInterval decode <*> decodeListWith decoder
    _ -> fail "invalide DataWithInterval encoding"

instance (Serialise a) => Serialise (DataWithInterval a) where
  encode = encodeDataWithInterval encode
  decode = decodeDataWithInterval decode

instance (FromJSVal a) => FromJSVal (DataWithInterval a) where
  fromJSVal o = do
    mT1 <- (fmap Finite) <$> (o ! "startTime" >>= fromJSVal)
    mT2 <- (fmap Finite) <$> (o ! "endTime" >>= fromJSVal)
    let mInterval = (<=..<=) <$> mT1 <*> mT2
    mData <- o ! "data" >>= fromJSVal
    pure $ DataWithInterval <$> mInterval <*> mData

data Lightning = Lightning
  { place :: StrictText, -- location
    occur :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSVal, Serialise)

data Rainfall = Rainfall
  { interval :: Interval (Length Scientific), -- Minimum & Maximum rainfall record
    place :: StrictText, -- location
    main :: Bool -- Maintenance flag (TRUE/FALSE)
  }
  deriving stock (Eq, Show)

instance Serialise Rainfall where
  encode (Rainfall interval' place main) = encodeListLen 4 <> encodeWord 0 <> encodeInterval (encodeScientific . (/~ milli meter)) interval' <> encode place <> encode main
  decode =
    (,) <$> decodeListLen <*> decodeWord >>= \case
      (4, 0) -> Rainfall <$> decodeInterval ((*~ milli meter) <$> decodeScientific) <*> decode <*> decode
      _ -> fail "invalid Rainfall encoding"

instance FromJSVal Rainfall where
  fromJSVal o = do
    mPlace <- o ! "place" >>= fromJSVal
    mMain <-
      o ! "main"
        >>= fromJSVal
        <&> ( >>=
                \case
                  "TRUE" -> Just True
                  "FALSE" -> Just False
                  _ -> Nothing
            )
    mUnit <-
      o ! "unit"
        >>= fromJSVal
        <&> ( >>=
                \case
                  "mm" -> Just $ milli meter
                  _ -> Nothing
            )
    mMin <-
      o ! "min" >>= fromJSVal <&> \case
        Just min' -> Finite . (min' *~) <$> mUnit
        Nothing -> Just NegInf
    mMax <-
      o ! "max" >>= fromJSVal <&> \case
        Just max' -> Finite . (max' *~) <$> mUnit
        Nothing -> Just PosInf
    pure $
      Rainfall
        <$> ((<=..<=) <$> mMin <*> mMax)
        <*> mPlace
        <*> mMain

data UVIndexData = UVIndexData
  { place :: StrictText, -- location
    value :: Scientific, -- value
    desc :: StrictText, -- description
    message :: Maybe StrictText -- message
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSVal)

instance Serialise UVIndexData where
  encode (UVIndexData place value desc message) = encodeListLen 5 <> encodeWord 0 <> encode place <> encodeScientific value <> encode desc <> encode message
  decode =
    (,) <$> decodeListLen <*> decodeWord >>= \case
      (5, 0) -> UVIndexData <$> decode <*> decodeScientific <*> decode <*> decode
      _ -> fail "invalid Rainfall encoding"

data UVIndex = UVIndex
  { _data :: [UVIndexData],
    recordDesc :: StrictText -- record description
  }
  deriving stock (Show, Eq)

instance Serialise UVIndex where
  encode (UVIndex _data recordDesc) = encodeListLen 3 <> encodeWord 0 <> encode _data <> encode recordDesc
  decode =
    (,) <$> decodeListLen <*> decodeWord >>= \case
      (3, 0) -> UVIndex <$> decode <*> decode
      _ -> fail "invalid UVIndex encoding"

instance FromJSVal UVIndex where
  fromJSVal o =
    (<|>)
      <$>
      -- NOTE: it just return empty string at night!! not mentioned in doc, so nice!
      ( fromJSVal o <&> \case
          Just "" -> Just $ UVIndex [] "nighttime no uv!"
          _ -> Nothing
      )
      <*> do
        mData <- o ! "data" >>= fromJSVal
        mRecordDesc <- o ! "recordDesc" >>= fromJSVal
        pure $ UVIndex <$> mData <*> mRecordDesc

data DataWithRecordTime a = DataWithRecordTime
  { recordTime :: UTCTime, -- record time YYYY-MMDD'T'hh:mm:ssZ Example: 2020-09- 01T08:19:00+08:00
    _data :: [a]
  }
  deriving stock (Eq, Show)

encodeDataWithRecordTime :: (a -> Encoding) -> DataWithRecordTime a -> Encoding
encodeDataWithRecordTime encoder (DataWithRecordTime recordTime _data) = encodeListLen 3 <> encodeWord 0 <> encode recordTime <> encodeListWith encoder _data

decodeDataWithRecordTime :: Decoder s a -> Decoder s (DataWithRecordTime a)
decodeDataWithRecordTime decoder =
  (,) <$> decodeListLen <*> decodeWord >>= \case
    (3, 0) -> DataWithRecordTime <$> decode <*> decodeListWith decoder
    _ -> fail "invalide DataWithRecordTime encoding"

instance (Serialise a) => Serialise (DataWithRecordTime a) where
  encode = encodeDataWithRecordTime encode
  decode = decodeDataWithRecordTime decode

instance (FromJSVal a) => FromJSVal (DataWithRecordTime a) where
  fromJSVal o = do
    mRecordTime <- o ! "recordTime" >>= fromJSVal
    mData <- o ! "data" >>= fromJSVal
    pure $ DataWithRecordTime <$> mRecordTime <*> mData

data Temperature = Temperature
  { place :: StrictText, -- location
    value :: ThermodynamicTemperature Scientific -- value
  }
  deriving stock (Eq, Show)

instance Serialise Temperature where
  encode (Temperature place value) = encodeListLen 3 <> encodeWord 0 <> encode place <> encodeScientific (value /~ degreeCelsius)
  decode =
    (,) <$> decodeListLen <*> decodeWord >>= \case
      (3, 0) -> Temperature <$> decode <*> ((*~ degreeCelsius) <$> decodeScientific)
      _ -> fail "invalid Temperature encoding"

instance FromJSVal Temperature where
  fromJSVal o = do
    mPlace <- o ! "place" >>= fromJSVal
    mValue <- parseJSVal_DegreeCelsius o
    pure $ Temperature <$> mPlace <*> mValue

data Humidity = Humidity
  { place :: StrictText, -- location
    value :: Dimensionless Scientific -- value
  }
  deriving stock (Eq, Show)

instance Serialise Humidity where
  encode (Humidity place value) = encodeListLen 3 <> encodeWord 0 <> encode place <> encodeScientific (value /~ percent)
  decode =
    (,) <$> decodeListLen <*> decodeWord >>= \case
      (3, 0) -> Humidity <$> decode <*> ((*~ percent) <$> decodeScientific)
      _ -> fail "invalid Humidity encoding"

instance FromJSVal Humidity where
  fromJSVal o = do
    mPlace <- o ! "place" >>= fromJSVal
    mValue <-
      parseJSVal_Unit
        ( \case
            "percent" -> Just percent
            _ -> Nothing
        )
        o
    pure $ Humidity <$> mPlace <*> mValue

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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Serialise)

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
