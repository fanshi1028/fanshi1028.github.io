{-# LANGUAGE TypeFamilies #-}

module Dashboard.DataSource.IO where

import Data.Hashable
import Data.Time
import Haxl.Core
import Haxl.DataSource.ConcurrentIO

data IOAction

instance ConcurrentIO IOAction where
  data ConcurrentIOReq IOAction a where
    GetCurrentTime :: ConcurrentIOReq IOAction UTCTime
    GetCurrentTimeZone :: ConcurrentIOReq IOAction TimeZone
  performIO = \case
    GetCurrentTime -> getCurrentTime
    GetCurrentTimeZone -> getCurrentTimeZone

deriving instance Eq (ConcurrentIOReq IOAction a)

deriving instance Show (ConcurrentIOReq IOAction a)

instance ShowP (ConcurrentIOReq IOAction) where showp = show

instance Hashable (ConcurrentIOReq IOAction a) where
  hashWithSalt s =
    hashWithSalt @Int s . \case
      GetCurrentTime -> 0
      GetCurrentTimeZone -> 1
