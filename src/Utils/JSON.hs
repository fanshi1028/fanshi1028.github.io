{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.JSON where

import Data.Functor
import Data.Hashable
import Data.List
import Data.Scientific
import Data.Text (StrictText, toLower)
import Data.Time
import Data.Time.Format.ISO8601
import Miso.DSL
import Miso.JSON
import Miso.String (fromMisoString)
import Numeric.Natural
import System.IO.Unsafe
import Text.ParserCombinators.ReadP

instance FromJSVal UTCTime where
  fromJSVal v = (>>= iso8601ParseM . fromMisoString) <$> fromJSVal v

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

instance FromJSVal Scientific where
  fromJSVal v =
    fromJSVal v
      <&> (>>= fmap fst . find ((== "") . snd) . readP_to_S scientificP)

instance FromJSVal Natural where
  fromJSVal v =
    fromJSVal v <&> \case
      Just x
        | x >= 0 -> Just $ toEnum x
        | otherwise -> Nothing
      Nothing -> Nothing

instance Hashable Value where
  hashWithSalt s = hashWithSalt s . encodePretty

instance Hashable JSVal where
  hashWithSalt s = hashWithSalt s . fromMisoString @StrictText . unsafePerformIO . jsonStringify

instance Show JSVal where
  show = fromMisoString . unsafePerformIO . jsonStringify
