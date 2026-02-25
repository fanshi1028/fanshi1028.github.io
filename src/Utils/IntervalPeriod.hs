{-# LANGUAGE NoImplicitPrelude #-}

module Utils.IntervalPeriod where

import Data.Hashable
import Data.Proxy
import Data.Time
import GHC.TypeNats
import Numeric.Units.Dimensional.Prelude

-- NOTE: ttl is assumed to be less than a Day, and a Day is consisted of multiple whole ttl time blocks
data IntervalPeriod (minutes :: Nat) = IntervalPeriod Day Natural
  deriving (Show, Eq)

instance Hashable (IntervalPeriod minutes) where
  hashWithSalt s (IntervalPeriod (ModifiedJulianDay d) n) = s `hashWithSalt` d `hashWithSalt` n

utcTimeToIntervalPeriod :: (KnownNat minutes) => Proxy minutes -> UTCTime -> IntervalPeriod minutes
utcTimeToIntervalPeriod proxy t =
  let ttl = changeRep $ natVal proxy *~ minute
      dayTime = (fromIntegral (diffTimeToPicoseconds $ utctDayTime t) *~ pico second)
   in IntervalPeriod (utctDay t) (floor @Rational $ dayTime / ttl /~ one)

type TwiceADay = IntervalPeriod 720

type OncePerDay = IntervalPeriod 1440

type OncePer30Days = IntervalPeriod 43200
