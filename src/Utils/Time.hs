{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Utils.Time (showRelativeTime, showTime, TimeData) where

import Data.Function
import Data.Time
import Data.Time.Format.ISO8601
import Miso.DSL hiding (Object)
import Miso.JSON
import Miso.String (fromMisoString)

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

instance FromJSVal TimeData where
  fromJSVal v = (>>= (fmap TimeData . iso8601ParseM . fromMisoString)) <$> fromJSVal v

showRelativeTime' :: UTCTime -> TimeData -> String
showRelativeTime' currentTime (TimeData t@(diffUTCTime currentTime . zonedTimeToUTC -> td))
  | td > nominalDay = show $ zonedTimeToLocalTime t
  | td < -nominalDay = show $ zonedTimeToLocalTime t
  | td < -60 * 60 = formatTime defaultTimeLocale "%h hours later" td
  | td > 60 * 60 = formatTime defaultTimeLocale "%h hours ago" td
  | td < 0 = formatTime defaultTimeLocale "%m mins later" td
  | otherwise = formatTime defaultTimeLocale "%m mins ago" td

showRelativeTime :: Maybe UTCTime -> TimeData -> String
showRelativeTime Nothing = showTime
showRelativeTime (Just currentTime) = showRelativeTime' currentTime

showTime :: TimeData -> String
showTime (TimeData t) = show $ zonedTimeToLocalTime t
