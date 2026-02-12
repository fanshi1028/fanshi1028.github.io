{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.JSON where

import Data.Functor
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

instance FromJSVal UTCTime where
  fromJSVal v = (>>= iso8601ParseM . fromMisoString) <$> fromJSVal v

instance ToJSVal UTCTime where
  toJSVal = toJSVal . iso8601Show

instance FromJSVal DayOfWeek where
  fromJSVal v =
    ( >>=
        \t -> case toLower t of
          "monday" -> Just Monday
          "tuesday" -> Just Tuesday
          "wednesday" -> Just Wednesday
          "thursday" -> Just Thursday
          "friday" -> Just Friday
          "saturday" -> Just Saturday
          "sunday" -> Just Sunday
          _ -> Nothing
    )
      <$> fromJSVal v

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

instance FromJSVal Scientific where
  fromJSVal v =
    fromJSVal v
      <&> (>>= fmap fst . find ((== "") . snd) . readP_to_S scientificP)

instance ToJSVal Scientific where
  toJSVal = toJSVal . show

instance FromJSVal Natural where
  fromJSVal v =
    fromJSVal v <&> \case
      Just x
        | x >= 0 -> Just $ toEnum x
        | otherwise -> Nothing
      Nothing -> Nothing

-- NOTE: HACK TEMP FIXME
instance Show JSVal where
  show = fromMisoString . unsafePerformIO . jsonStringify

instance (FromJSVal a) => FromJSVal (V.Vector a) where
  fromJSVal v = fmap V.fromList <$> fromJSVal v

instance (ToJSVal a) => ToJSVal (V.Vector a) where
  toJSVal = toJSVal . V.toList

instance ToJSVal Natural where
  toJSVal = toJSVal @Int . fromIntegral
