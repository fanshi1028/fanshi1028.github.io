{-# LANGUAGE NoImplicitPrelude #-}

module Utils.IntervalPeriod where

import Data.Proxy
import Data.Time
import GHC.TypeNats
import Numeric.Units.Dimensional.Prelude

-- NOTE: ttl is assumed to be less than a Day, and a Day is consisted of multiple whole ttl time blocks
data IntervalPeriod (minutes :: Nat) = IntervalPeriod Day Natural

utcTimeToIntervalPeriod :: (KnownNat minutes) => Proxy minutes -> UTCTime -> IntervalPeriod minutes
utcTimeToIntervalPeriod proxy t =
  let ttl = changeRep $ natVal proxy *~ minute
      dayTime = (fromIntegral (diffTimeToPicoseconds $ utctDayTime t) *~ pico second)
   in IntervalPeriod (utctDay t) (floor @Rational $ dayTime / ttl /~ one)
