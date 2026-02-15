{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.JSON where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Function
import Data.Scientific
import Data.Text (toLower)
import Data.Time
import Data.Time.Format.ISO8601
import Data.Vector qualified as V
import Miso.Aeson
import Miso.DSL hiding (Object)
import Miso.JSON
import Miso.String (MisoString, fromMisoString, ms)
import Numeric.Natural
import System.IO.Unsafe

newtype TimeData = TimeData ZonedTime
  deriving (Show)

instance Eq TimeData where
  (==) (TimeData a) (TimeData b) = ((==) `on` zonedTimeToUTC) a b

instance Ord TimeData where
  compare (TimeData a) (TimeData b) = (compare `on` zonedTimeToUTC) a b

instance FromJSON TimeData where
  parseJSON v = TimeData <$> withText "TimeData" (iso8601ParseM . fromMisoString) v

instance ToJSVal TimeData where
  toJSVal (TimeData zt) = toJSVal $ iso8601Show zt

instance FromJSON DayOfWeek where
  parseJSON = withText "DayOfWeek" $ \t -> case toLower $ fromMisoString t of
    "monday" -> pure Monday
    "tuesday" -> pure Tuesday
    "wednesday" -> pure Wednesday
    "thursday" -> pure Thursday
    "friday" -> pure Friday
    "saturday" -> pure Saturday
    "sunday" -> pure Sunday
    _ -> typeMismatch "DayOfWeek" $ String t

instance ToJSVal DayOfWeek where
  toJSVal =
    toJSVal @MisoString . \case
      Monday -> "monday"
      Tuesday -> "tuesday"
      Wednesday -> "wednesday"
      Thursday -> "thursday"
      Friday -> "friday"
      Saturday -> "saturday"
      Sunday -> "sunday"

deriving via (UnAesonised Scientific) instance FromJSON Scientific

instance ToJSVal Scientific where
  toJSVal = toJSVal_Value . aesonToJSON . Aeson.toJSON

-- NOTE: HACK TEMP FIXME
instance Show JSVal where
  show = fromMisoString . unsafePerformIO . jsonStringify

instance (FromJSVal a) => FromJSVal (V.Vector a) where
  fromJSVal v = fmap V.fromList <$> fromJSVal v

instance (ToJSVal a) => ToJSVal (V.Vector a) where
  toJSVal = toJSVal . V.toList

instance ToJSVal Natural where
  toJSVal = toJSVal @Int . fromIntegral

-- NOTE: HACK TEMP FIXME
instance FromJSON JSVal where
  parseJSON = pure . unsafePerformIO . toJSVal_Value

newtype Aesonised a = Aesonised a

instance (FromJSON a) => Aeson.FromJSON (Aesonised a) where
  parseJSON (aesonToJSON -> v) = case parseEither parseJSON v of
    Left err -> Aeson.parseFail $ fromMisoString err
    Right r -> pure $ Aesonised r

newtype UnAesonised a = UnAesonised a

instance (Aeson.FromJSON a) => FromJSON (UnAesonised a) where
  parseJSON v'@(jsonToAeson -> v) = case Aeson.parseEither Aeson.parseJSON v of
    Left err -> typeMismatch (ms err) v'
    Right r -> pure $ UnAesonised r
