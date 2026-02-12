{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.JSON where

import Data.List
import Data.Scientific
import Data.Text (toLower)
import Data.Time
import Data.Time.Format.ISO8601
import Data.Vector qualified as V
import Miso.DSL
import Miso.JSON
import Miso.String (MisoString, fromMisoString)
import Numeric.Natural
import System.IO.Unsafe
import Text.ParserCombinators.ReadP

instance FromJSON UTCTime where
  parseJSON v = parseJSON v >>= iso8601ParseM . fromMisoString

instance ToJSVal UTCTime where
  toJSVal = toJSVal . iso8601Show

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

instance FromJSON Scientific where
  parseJSON v = do
    v' <- fromMisoString <$> parseJSON v
    case find ((== "") . snd) $ readP_to_S scientificP v' of
      Nothing -> typeMismatch "Scientific" v
      Just (r, _) -> pure r

instance ToJSVal Scientific where
  toJSVal = toJSVal . show


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
