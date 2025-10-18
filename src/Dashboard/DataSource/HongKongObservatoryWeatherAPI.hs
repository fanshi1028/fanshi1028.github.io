{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Dashboard.DataSource.HongKongObservatoryWeatherAPI where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (throw)
import Control.Lens.Setter hiding ((*~), (.=))
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.KeyMap qualified as AKM
import Data.Aeson.Types
import Data.Function
import Data.Functor
import Data.Hashable
import Data.Interval
import Data.Text hiding (concat, elem, foldl', show)
import Data.Text qualified as T
import Data.Time
import Data.Typeable
import Data.Void
import GHC.Generics
import Haxl.Core hiding (throw)
import Language.Javascript.JSaddle hiding (Object, Success)
import Miso hiding (URI, defaultOptions, on)
import Miso.FFI qualified as FFI
import Network.URI
import Network.URI.Lens
import Numeric.Natural
import Numeric.Units.Dimensional
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

data LocalWeatherForecast = LocalWeatherForecast
  { generalSituation :: StrictText, -- General Situation
    tcInfo :: StrictText, -- Tropical Cyclone Information
    fireDangerWarning :: StrictText, -- Fire Danger Warning Message
    forecastPeriod :: StrictText, -- Forecast Period
    forecastDesc :: StrictText, -- Forecast Description
    outlook :: StrictText, -- Outlook
    updateTime :: UTCTime -- Update Time YYYY-MM-DD'T'hh:mm:ssZ Example: 2020-09-01T08:19:00+08:00
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

instance FromJSVal LocalWeatherForecast where
  fromJSVal = fromJSValViaValue Proxy

data SoilTemp = SoilTemp
  { place :: StrictText, -- location
    value :: ThermodynamicTemperature Float, -- value
    recordTime :: UTCTime, -- record time YYYY-MMDD'T'hh:mm:ssZ Example: 2020-09- 01T08:19:00+08:00
    depth :: Length Float
  }
  deriving stock (Show)

instance FromJSON SoilTemp where
  parseJSON = withObject "SoilTemp" $ \o ->
    SoilTemp
      <$> o .: "place"
      <*> ( (*~)
              <$> o .: "value"
              <*> ( o .: "unit" >>= \case
                      "C" -> pure degreeCelsius
                      txt -> fail $ "unexpected temperature unit: " <> txt
                  )
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
  deriving stock (Show)

instance FromJSON SeaTemp where
  parseJSON = withObject "SeaTemp" $ \o ->
    SeaTemp
      <$> o .: "place"
      <*> ( (*~)
              <$> o .: "value"
              <*> ( o .: "unit" >>= \case
                      "C" -> pure degreeCelsius
                      txt -> fail $ "unexpected temperature unit: " <> txt
                  )
          )
      <*> o .: "recordTime"

data ValueWithUnit = ValueWithUnit
  { value :: Float,
    unit :: StrictText
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

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
    --    Medium                                                                                  --
    --    High Medium                                                                             --
    --    Medium Low Low                                                                          --
    -- Response value description: https://www.hko.gov.hk/en/wxinfo/currwx/fnd.htm?tablenote=true --
    ------------------------------------------------------------------------------------------------
    psr :: StrictText, -- TEMP FIXME
    forecastIcon :: Natural
  }
  deriving stock (Show)

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
                            (*~)
                              <$> o' .: "value"
                              <*> ( o' .: "unit" >>= \case
                                      "C" -> pure degreeCelsius
                                      txt -> fail $ "unexpected temperature unit: " <> txt
                                  )
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
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

instance FromJSVal NineDayWeatherForecast where
  fromJSVal = fromJSValViaValue Proxy

data DataWithInterval a = DataWithInterval
  { --  Start Time YYYY-MM-DD'T'hh:mm:ssZ Example: 2020-09-01T08:19:00+08:00 endTime End Time
    --  End Time YYYY-MM-DD'T'hh:mm:ssZ Example: 2020-09-01T08:19:00+08:00 endTime End Time
    interval :: Interval UTCTime,
    _data :: [a]
  }
  deriving stock (Show)

instance (FromJSON a) => FromJSON (DataWithInterval a) where
  parseJSON = withObject "DataWithInterval" $ \o -> do
    t1 <- Finite <$> o .: "startTime"
    t2 <- Finite <$> o .: "endTime"
    DataWithInterval (t1 <=..<= t2) <$> o .: "data"

data Lightning = Lightning
  { place :: StrictText, -- location
    occur :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data Rainfall = Rainfall
  { interval :: Interval (Length Float), -- Minimum & Maximum rainfall record
    place :: StrictText, -- location
    main :: Bool -- Maintenance flag (TRUE/FALSE)
  }
  deriving stock (Show)

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
  deriving anyclass (FromJSON, ToJSON)

data UVIndex = UVIndex
  { _data :: [UVIndexData],
    recordDesc :: StrictText -- record description
  }
  deriving stock (Show, Eq)

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
  deriving stock (Show)

instance (FromJSON a) => FromJSON (DataWithRecordTime a) where
  parseJSON = withObject "DataWithRecordTime" $ \o -> do
    DataWithRecordTime <$> o .: "recordTime" <*> o .: "data"

data Temperature = Temperature
  { place :: StrictText, -- location
    value :: ThermodynamicTemperature Float -- value
  }
  deriving stock (Show)

instance FromJSON Temperature where
  parseJSON = withObject "Temperature" $ \o ->
    Temperature
      <$> o .: "place"
      <*> ( (*~)
              <$> o .: "value"
              <*> ( o .: "unit" >>= \case
                      "C" -> pure degreeCelsius
                      txt -> fail $ "unexpected temperature unit: " <> txt
                  )
          )

data Humidity = Humidity
  { place :: StrictText, -- location
    value :: Dimensionless Float -- value
  }
  deriving stock (Show)

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
  deriving stock (Show, Generic)

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
  GetLocalWeaterForecast :: HKOWeatherInformationReq LocalWeatherForecast
  Get9DayWeatherForecast :: HKOWeatherInformationReq NineDayWeatherForecast
  GetCurrentWeatherReport :: HKOWeatherInformationReq CurrentWeatherReport
  GetWeatherWarningSummary :: HKOWeatherInformationReq Value
  GetWeatherWarningInfo :: HKOWeatherInformationReq Value
  GetSpecialWeatherTips :: HKOWeatherInformationReq Value

deriving instance Eq (HKOWeatherInformationReq a)

instance Hashable (HKOWeatherInformationReq a) where
  hashWithSalt s req = hashWithSalt @Int s $ case req of
    GetLocalWeaterForecast -> 0
    Get9DayWeatherForecast -> 1
    GetCurrentWeatherReport -> 2
    GetWeatherWarningSummary -> 3
    GetWeatherWarningInfo -> 4
    GetSpecialWeatherTips -> 5

deriving instance Show (HKOWeatherInformationReq a)

instance ShowP HKOWeatherInformationReq where showp = show

instance StateKey HKOWeatherInformationReq where
  data State HKOWeatherInformationReq = HKOWeatherInformationReqState

instance DataSourceName HKOWeatherInformationReq where
  dataSourceName _ = "HKO Weather Information API"

hkoWeatherInformationReqToURI :: HKOWeatherInformationReq a -> URI
hkoWeatherInformationReqToURI req =
  URI
    "https:"
    (Just $ nullURIAuth & uriRegNameLens .~ "data.weather.gov.hk")
    "/weatherAPI/opendata/weather.php"
    ( "?dataType=" <> case req of
        GetLocalWeaterForecast -> "flw"
        Get9DayWeatherForecast -> "fnd"
        GetCurrentWeatherReport -> "rhrread"
        GetWeatherWarningSummary -> "warnsum"
        GetWeatherWarningInfo -> "warninginfo"
        GetSpecialWeatherTips -> "set"
    )
    ""

instance DataSource JSContextRef HKOWeatherInformationReq where
  fetch reqState flags javaScriptContext =
    backgroundFetchPar
      ( -- NOTE: sad boilerplate
        \req -> case req of
          GetLocalWeaterForecast -> handler req
          Get9DayWeatherForecast -> handler req
          GetCurrentWeatherReport -> handler req
          GetWeatherWarningSummary -> handler req
          GetWeatherWarningInfo -> handler req
          GetSpecialWeatherTips -> handler req
      )
      reqState
      flags
      javaScriptContext
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
        runJSM (FFI.fetch url "GET" Nothing [] successCB failCB JSON) javaScriptContext
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
