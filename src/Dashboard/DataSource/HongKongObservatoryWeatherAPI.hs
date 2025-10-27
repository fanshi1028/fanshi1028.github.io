{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dashboard.DataSource.HongKongObservatoryWeatherAPI where

import Codec.CBOR.JSON
import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (throw)
import Control.Lens.Setter hiding ((*~), (.=))
import Control.Monad.IO.Class
import Data.Aeson hiding (Encoding, decode, encode)
import Data.Aeson.Key
import Data.Aeson.KeyMap qualified as AKM
import Data.Aeson.Types hiding (Encoding)
import Data.Function
import Data.Functor
import Data.Hashable
import Data.Interval
import Data.Maybe
import Data.Text hiding (concat, elem, foldl', foldr, reverse, show)
import Data.Text qualified as T
import Data.Time
import Data.Typeable
import Data.Void
import GHC.Generics
import Haxl.Core hiding (throw)
import Language.Javascript.JSaddle hiding (Object, Success)
import Miso hiding (Decoder, URI, defaultOptions, on)
import Miso.FFI qualified as FFI
import Network.URI
import Network.URI.Lens
import Numeric.Natural
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.Coercion
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.SIUnits
import UnliftIO.Exception

{-# WARNING fromJSValViaValue "partial, throw error when JSON assumption is wrong" #-}
fromJSValViaValue :: (FromJSON a, Typeable a) => Proxy a -> JSVal -> JSM (Maybe a)
fromJSValViaValue (typeRepTyCon . typeRep -> tyCon) a =
  fromJSVal a <&> \mv ->
    ifromJSON <$> mv >>= \case
      ISuccess r -> Just r
      IError path' err ->
        -- HACK
        throw . JSONError . T.pack $
          concat
            [ "Error in ",
              tyConModule tyCon,
              "(",
              tyConName tyCon,
              formatRelativePath path',
              "):",
              err
            ]

newtype SerialisableValue = SerialisableValue Value deriving newtype (Eq, Show, FromJSON)

instance FromJSVal SerialisableValue where
  fromJSVal v = fmap SerialisableValue <$> fromJSVal v

instance Serialise SerialisableValue where
  encode (SerialisableValue v) = encodeValue v
  decode = SerialisableValue <$> decodeValue False

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
    value :: ThermodynamicTemperature Float, -- value
    recordTime :: UTCTime, -- record time YYYY-MMDD'T'hh:mm:ssZ Example: 2020-09- 01T08:19:00+08:00
    depth :: Length Float
  }
  deriving stock (Eq, Show)

instance Serialise SoilTemp where
  encode (SoilTemp place value recordTime depth) = encodeListLen 5 <> encodeWord 0 <> encode place <> encode (toDegreeCelsiusAbsolute value) <> encode recordTime <> encode (unQuantity depth)
  decode =
    (,) <$> decodeListLen <*> decodeWord >>= \case
      (5, 0) -> SoilTemp <$> decode <*> (fromDegreeCelsiusAbsolute <$> decode) <*> decode <*> ((*~ meter) <$> decode)
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
    value :: ThermodynamicTemperature Float, -- value
    recordTime :: UTCTime -- record time YYYY-MMDD'T'hh:mm:ssZ Example: 2020-09- 01T08:19:00+08:00
  }
  deriving stock (Eq, Show)

instance Serialise SeaTemp where
  encode (SeaTemp place value recordTime) = encodeListLen 4 <> encodeWord 0 <> encode place <> encode (toDegreeCelsiusAbsolute value) <> encode recordTime
  decode =
    (,) <$> decodeListLen <*> decodeWord >>= \case
      (4, 0) -> SeaTemp <$> decode <*> (fromDegreeCelsiusAbsolute <$> decode) <*> decode
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
    forecastTempInterval :: Interval (ThermodynamicTemperature Float), -- Forecast Temperature
    forecastRHInterval :: Interval (Dimensionless Float), -- Forecast  Relative Humidity
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

encodeExtended :: (a -> Encoding) -> Extended a -> Encoding
encodeExtended encoder = \case
  NegInf -> encodeListLen 1 <> encodeWord 0
  Finite a -> encodeListLen 2 <> encodeWord 1 <> encoder a
  PosInf -> encodeListLen 1 <> encodeWord 3

decodeExtended :: Decoder s a -> Decoder s (Extended a)
decodeExtended decoder =
  (,) <$> decodeListLen <*> decodeWord >>= \case
    (1, 0) -> pure NegInf
    (1, 3) -> pure PosInf
    (2, 1) -> Finite <$> decoder
    _ -> fail "invalid Extended encoding"

encodeBoundary :: Boundary -> Encoding
encodeBoundary = \case
  Open -> encodeWord 0
  Closed -> encodeWord 1

decodeBoundary :: Decoder s Boundary
decodeBoundary =
  decodeWord >>= \case
    0 -> pure Open
    1 -> pure Closed
    _ -> fail "invalid Boundary encoding"

encodeInterval :: (a -> Encoding) -> Interval a -> Encoding
encodeInterval encoder interval' =
  let (lb, lbBoundary) = lowerBound' interval'
      (ub, ubBoundary) = upperBound' interval'
   in encodeListLen 5 <> encodeWord 0 <> encodeExtended encoder lb <> encodeBoundary lbBoundary <> encodeExtended encoder ub <> encodeBoundary ubBoundary

decodeInterval :: (Ord a) => Decoder s a -> Decoder s (Interval a)
decodeInterval decoder =
  (,) <$> decodeListLen <*> decodeWord >>= \case
    (5, 0) -> interval <$> ((,) <$> decodeExtended decoder <*> decodeBoundary) <*> ((,) <$> decodeExtended decoder <*> decodeBoundary)
    _ -> fail "invalid Interval encoding"

instance Serialise WeatherForecast where
  encode (WeatherForecast forcastDate week forecastWind forecastWeather forecastTempInterval forecastRHInterval psr forecastIcon) =
    encodeListLen 9
      <> encodeWord 0
      <> encode (toModifiedJulianDay forcastDate)
      <> encode (fromEnum week)
      <> encode forecastWind
      <> encode forecastWeather
      <> encodeInterval (encode . toDegreeCelsiusAbsolute) forecastTempInterval
      <> encodeInterval (encode . unQuantity) forecastRHInterval
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
          <*> decodeInterval (fromDegreeCelsiusAbsolute <$> decode)
          <*> decodeInterval ((*~ percent) <$> decode)
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
  { interval :: Interval (Length Float), -- Minimum & Maximum rainfall record
    place :: StrictText, -- location
    main :: Bool -- Maintenance flag (TRUE/FALSE)
  }
  deriving stock (Eq, Show)

instance Serialise Rainfall where
  encode (Rainfall interval' place main) = encodeListLen 4 <> encodeWord 0 <> encodeInterval (encode . unQuantity) interval' <> encode place <> encode main
  decode =
    (,) <$> decodeListLen <*> decodeWord >>= \case
      (4, 0) -> Rainfall <$> decodeInterval ((*~ milli meter) <$> decode) <*> decode <*> decode
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
    value :: Float, -- value
    desc :: StrictText, -- description
    message :: Maybe StrictText -- message
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, Serialise)

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
  toEncoding uvi = pairs $ "data" .= uvi._data <> "recordDesc" .= uvi.recordDesc
  toJSON uvi = object ["data" .= uvi._data, "recordDesc" .= uvi.recordDesc]

data DataWithRecordTime a = DataWithRecordTime
  { recordTime :: UTCTime, -- record time YYYY-MMDD'T'hh:mm:ssZ Example: 2020-09- 01T08:19:00+08:00
    _data :: [a]
  }
  deriving stock (Eq, Show)

-- NOTE: ref defaultEncodeList
encodeListWith :: (a -> Encoding) -> [a] -> Encoding
encodeListWith _ [] = encodeListLen 0
encodeListWith encoder ls = encodeListLenIndef <> foldr (\x r -> encoder x <> r) encodeBreak ls

-- NOTE: ref defaultDecodeList
decodeListWith :: Decoder s a -> Decoder s [a]
decodeListWith decoder =
  decodeListLenOrIndef >>= \case
    Nothing -> decodeSequenceLenIndef (flip (:)) [] reverse decoder
    Just n -> decodeSequenceLenN (flip (:)) [] reverse n decoder

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
    value :: ThermodynamicTemperature Float -- value
  }
  deriving stock (Eq, Show)

instance Serialise Temperature where
  encode (Temperature place value) = encodeListLen 3 <> encodeWord 0 <> encode place <> encode (toDegreeCelsiusAbsolute value)
  decode =
    (,) <$> decodeListLen <*> decodeWord >>= \case
      (3, 0) -> Temperature <$> decode <*> (fromDegreeCelsiusAbsolute <$> decode)
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
    value :: Dimensionless Float -- value
  }
  deriving stock (Eq, Show)

instance Serialise Humidity where
  encode (Humidity place value) = encodeListLen 3 <> encodeWord 0 <> encode place <> encode (unQuantity value)
  decode =
    (,) <$> decodeListLen <*> decodeWord >>= \case
      (3, 0) -> Humidity <$> decode <*> ((*~ percent) <$> decode)
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

-- NOTE: Weather Information API
data HKOWeatherInformationReq a where
  GetLocalWeatherForecast :: HKOWeatherInformationReq LocalWeatherForecast
  Get9DayWeatherForecast :: HKOWeatherInformationReq NineDayWeatherForecast
  GetCurrentWeatherReport :: HKOWeatherInformationReq CurrentWeatherReport
  GetWeatherWarningSummary :: HKOWeatherInformationReq SerialisableValue
  GetWeatherWarningInfo :: HKOWeatherInformationReq SerialisableValue
  GetSpecialWeatherTips :: HKOWeatherInformationReq SerialisableValue

deriving instance Eq (HKOWeatherInformationReq a)

instance Hashable (HKOWeatherInformationReq a) where
  hashWithSalt s req = hashWithSalt @Int s $ case req of
    GetLocalWeatherForecast -> 0
    Get9DayWeatherForecast -> 1
    GetCurrentWeatherReport -> 2
    GetWeatherWarningSummary -> 3
    GetWeatherWarningInfo -> 4
    GetSpecialWeatherTips -> 5

deriving instance Show (HKOWeatherInformationReq a)

instance ShowP HKOWeatherInformationReq where showp = show

instance StateKey HKOWeatherInformationReq where
  newtype State HKOWeatherInformationReq = HKOWeatherInformationReqState JSContextRef

instance DataSourceName HKOWeatherInformationReq where
  dataSourceName _ = "HKO Weather Information API"

hkoWeatherInformationReqToURI :: HKOWeatherInformationReq a -> URI
hkoWeatherInformationReqToURI req =
  URI
    "https:"
    (Just $ nullURIAuth & uriRegNameLens .~ "data.weather.gov.hk")
    "/weatherAPI/opendata/weather.php"
    ( "?dataType=" <> case req of
        GetLocalWeatherForecast -> "flw"
        Get9DayWeatherForecast -> "fnd"
        GetCurrentWeatherReport -> "rhrread"
        GetWeatherWarningSummary -> "warnsum"
        GetWeatherWarningInfo -> "warninginfo"
        GetSpecialWeatherTips -> "swt"
    )
    ""

instance DataSource u HKOWeatherInformationReq where
  fetch reqState@(HKOWeatherInformationReqState jscontext) =
    backgroundFetchPar
      ( -- NOTE: sad boilerplate
        \req -> case req of
          GetLocalWeatherForecast -> handler req
          Get9DayWeatherForecast -> handler req
          GetCurrentWeatherReport -> handler req
          GetWeatherWarningSummary -> handler req
          GetWeatherWarningInfo -> handler req
          GetSpecialWeatherTips -> handler req
      )
      reqState
    where
      handler :: (forall a. (FromJSVal a) => HKOWeatherInformationReq a -> IO (Either SomeException a))
      handler req = do
        successMVar <- newEmptyMVar
        failMVar <- newEmptyMVar
        let url = ms $ uriToString id (hkoWeatherInformationReqToURI req) ""
            successCB = \(Response _ _ _ v) -> liftIO $ putMVar successMVar v
            failCB =
              liftIO . putMVar failMVar . toException . \case
                Response Nothing _ _ _ -> FetchError "CORS or Network Error"
                Response (Just code) headers mErrMsg (v :: Value)
                  | code < 300 -> do
                      FetchError $ "TEMP FIXME ?????: " <> intercalate ", " [T.show code, T.show headers, T.show mErrMsg, T.show v]
                  | code < 400 -> FetchError $ "TEMP FIXME Redirect: " <> intercalate ", " [T.show code, T.show headers, T.show mErrMsg, T.show v]
                  | code < 500 -> FetchError $ "TEMP FIXME Client Error: " <> intercalate ", " [T.show code, T.show headers, T.show mErrMsg, T.show v]
                  | otherwise -> FetchError $ "Server Error: " <> intercalate ", " [T.show code, T.show headers, T.show mErrMsg, T.show v]
        runJSM (FFI.fetch url "GET" Nothing [] successCB failCB JSON) jscontext
        race (readMVar failMVar) (readMVar successMVar)

-- NOTE: Earthquake Information API
data HKOEarthquakeInformationReq a where
  GetQuickEarthquakeMessages :: HKOEarthquakeInformationReq Void
  GetLocallyFeltEarthTremor :: HKOEarthquakeInformationReq Void

data HKOOpenDataReq a where
  -- NOTE: Open Data (Climate and Weather Information) API
  GetHourlyHeightsOfAstronomicalTides :: HKOOpenDataReq Void
  GetTimesAndHeightsOfAstronomicalHighAndLowTides :: HKOOpenDataReq Void
  GetTimesOfSunriseSunTransitAndSunset :: HKOOpenDataReq Void
  TimesOfMoonriseMoonTransitAndMoonset :: HKOOpenDataReq Void
  GetGregorianLunarCalendarConversionTable :: HKOOpenDataReq Void
  GetCloudToGroundAndCloudToCloudLightningCount :: HKOOpenDataReq Void
  GetLatest10MinuteMeanVisibility :: HKOOpenDataReq Void
  GetDailyMeanTemperature :: HKOOpenDataReq Void
  GetDailyMaximumTemperature :: HKOOpenDataReq Void
  GetDailyMinimumTemperature :: HKOOpenDataReq Void
  GetWeatherAndRadiationLevelReport :: HKOOpenDataReq Void

-- NOTE: Gregorian-Lunar Calendar Conversion API
data HKOGregorianlLunarConversionReq a where
  GetLunarDate :: HKOGregorianlLunarConversionReq Void

-- NOTE: Rainfall in The Past Hour from Automatic Weather Station API
data HKOHourlyRainFallReq a where
  GetHourlyRainFall :: HKOHourlyRainFallReq Void
