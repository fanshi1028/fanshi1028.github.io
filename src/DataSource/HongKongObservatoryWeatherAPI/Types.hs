{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module DataSource.HongKongObservatoryWeatherAPI.Types where

import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Control.Applicative
import Data.Aeson hiding (Encoding, decode, encode)
import Data.Aeson.Key
import Data.Aeson.KeyMap qualified as AKM
import Data.Aeson.Types hiding (Encoding)
import Data.Function
import Data.Functor
import Data.Interval
import Data.Scientific
import Data.Text hiding (concat, elem, foldl', foldr, reverse, show)
import Data.Text qualified as T
import Data.Time
import Data.Typeable
import GHC.Generics
import Language.Javascript.JSaddle hiding (Object, Success)
import Numeric.Natural
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.SIUnits hiding (fromDegreeCelsiusAbsolute)
import Utils.Dimensional
import Utils.JS
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
  deriving anyclass (FromJSON, Serialise)

instance FromJSVal LocalWeatherForecast where
  fromJSVal = fromJSValViaValue Proxy

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

instance FromJSON SoilTemp where
  parseJSON = withObject "SoilTemp" $ \o ->
    SoilTemp
      <$> o .: "place"
      <*> ( ( o .: "unit" >>= \case
                "C" -> pure fromDegreeCelsiusAbsolute
                txt -> fail $ "unexpected temperature unit: " <> txt
            )
              <*> o .: "value"
          )
      <*> o .: "recordTime"
      <*> ( o .: "depth"
              >>= withObject
                "depth"
                ( \depth ->
                    (*~)
                      <$> depth .: "value"
                      <*> ( depth .: "unit" >>= \case
                              "metre" -> pure meter
                              txt -> fail $ "unexpected length unit: " <> txt
                          )
                )
          )

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

instance FromJSON SeaTemp where
  parseJSON = withObject "SeaTemp" $ \o ->
    SeaTemp
      <$> o .: "place"
      <*> ( ( o .: "unit" >>= \case
                "C" -> pure fromDegreeCelsiusAbsolute
                txt -> fail $ "unexpected temperature unit: " <> txt
            )
              <*> o .: "value"
          )
      <*> o .: "recordTime"

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

instance FromJSON WeatherForecast where
  parseJSON = withObject "WeatherForecast" $ \o ->
    WeatherForecast
      <$> (o .: "forecastDate" >>= parseTimeM @_ @Day False defaultTimeLocale "%Y%m%d")
      <*> o .: "week"
      <*> o .: "forecastWind"
      <*> o .: "forecastWeather"
      <*> ( do
              let parseValueWithUnit k =
                    o .: fromString k
                      >>= withObject
                        k
                        ( \o' ->
                            ( o' .: "unit" >>= \case
                                "C" -> pure fromDegreeCelsiusAbsolute
                                txt -> fail $ "unexpected temperature unit: " <> txt
                            )
                              <*> o' .: "value"
                        )
              min' <- parseValueWithUnit "forecastMintemp"
              max' <- parseValueWithUnit "forecastMaxtemp"
              pure $ Finite min' <=..<= Finite max'
          )
      <*> ( do
              let parseValueWithUnit k =
                    o .: fromString k
                      >>= withObject
                        k
                        ( \o' ->
                            (*~)
                              <$> o' .: "value"
                              <*> ( o' .: "unit" >>= \case
                                      "percent" -> pure percent
                                      txt -> fail $ "unexpected relative humidity unit: " <> txt
                                  )
                        )
              min' <- parseValueWithUnit "forecastMinrh"
              max' <- parseValueWithUnit "forecastMaxrh"
              pure $ Finite min' <=..<= Finite max'
          )
      <*> ( o .: "PSR" >>= \case
              txt
                | txt `elem` ["High", "Medium High", "Medium", "Medium Low", "Low"] -> pure txt -- TEMP FIXME
                | otherwise -> fail $ "unexpected PSR value: " <> T.unpack txt
          )
      <*> o .: "ForecastIcon"

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
  deriving anyclass (FromJSON, Serialise)

instance FromJSVal NineDayWeatherForecast where
  fromJSVal = fromJSValViaValue Proxy

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

instance (FromJSON a) => FromJSON (DataWithInterval a) where
  parseJSON = withObject "DataWithInterval" $ \o -> do
    t1 <- Finite <$> o .: "startTime"
    t2 <- Finite <$> o .: "endTime"
    DataWithInterval (t1 <=..<= t2) <$> o .: "data"

data Lightning = Lightning
  { place :: StrictText, -- location
    occur :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, Serialise)

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

instance FromJSON Rainfall where
  parseJSON = withObject "Rainfall" $ \o -> do
    unit <-
      o .: "unit" >>= \case
        "mm" -> pure $ milli meter
        txt -> fail $ "unexpected length unit: " <> txt
    min' <- (fmap (Finite . (*~ unit)) <$> o .:? "min") .!= NegInf
    max' <- (fmap (Finite . (*~ unit)) <$> o .:? "max") .!= PosInf
    Rainfall (min' <=..<= max')
      <$> o .: "place"
      <*> ( o .: "main" >>= \case
              "TRUE" -> pure True
              "FALSE" -> pure False
              txt -> fail $ "expected String: TRUE | FALSE but got " <> T.unpack txt
          )

data UVIndexData = UVIndexData
  { place :: StrictText, -- location
    value :: Scientific, -- value
    desc :: StrictText, -- description
    message :: Maybe StrictText -- message
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

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

instance FromJSON UVIndex where
  parseJSON v =
    ( withObject
        "UVIndex"
        ( \o ->
            UVIndex <$> o .: "data" <*> o .: "recordDesc"
        )
        v
    )
      -- NOTE: it just return empty string at night!! not mentioned in doc, so nice!
      <|> ( withText
              "UVIndex"
              ( \case
                  "" -> pure $ UVIndex [] "nighttime no uv!"
                  txt -> fail $ "unexpected! got nonempty string as uvindex: " <> unpack txt
              )
              v
          )

instance ToJSON UVIndex where
  toEncoding (UVIndex data' recordDesc) = pairs $ "data" .= data' <> "recordDesc" .= recordDesc
  toJSON (UVIndex data' recordDesc) = object ["data" .= data', "recordDesc" .= recordDesc]

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

instance (FromJSON a) => FromJSON (DataWithRecordTime a) where
  parseJSON = withObject "DataWithRecordTime" $ \o -> do
    DataWithRecordTime <$> o .: "recordTime" <*> o .: "data"

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

instance FromJSON Temperature where
  parseJSON = withObject "Temperature" $ \o ->
    Temperature
      <$> o .: "place"
      <*> ( ( o .: "unit" >>= \case
                "C" -> pure fromDegreeCelsiusAbsolute
                txt -> fail $ "unexpected temperature unit: " <> txt
            )
              <*> o .: "value"
          )

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

instance FromJSON Humidity where
  parseJSON = withObject "Humidity" $ \o ->
    Humidity
      <$> o .: "place"
      <*> ( (*~)
              <$> o .: "value"
              <*> ( o .: "unit" >>= \case
                      "percent" -> pure percent
                      txt -> fail $ "unexpected humidity unit: " <> txt
                  )
          )

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

instance FromJSON CurrentWeatherReport where
  parseJSON = withObject "CurrentWeatherReport" $ \o -> do
    let noArrayDataAsEmptyString k =
          ( o .: k >>= \case
              "" -> pure $ AKM.insert k emptyArray
              txt -> fail $ "expected empty string but got " <> T.unpack txt
          )
            <|> pure id
    objectFixes <-
      sequenceA
        [ noArrayDataAsEmptyString "tcmessage",
          noArrayDataAsEmptyString "warningMessage",
          o .:? "specialWxTips" <&> \case
            Nothing -> AKM.insert "specialWxTips" emptyArray
            Just (_ :: [StrictText]) -> id
        ]
    genericParseJSON defaultOptions . Object $ foldl' (&) o objectFixes

instance FromJSVal CurrentWeatherReport where
  fromJSVal = fromJSValViaValue Proxy
