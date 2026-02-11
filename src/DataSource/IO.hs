{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module DataSource.IO (ConcurrentIOReq (..)) where

import Component.Foreign.MapLibre
import Control.Concurrent
import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Data.Hashable
import Data.Time
import Haxl.Core hiding (throw)
import Haxl.DataSource.ConcurrentIO
import Miso.DSL
import Network.URI
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.SIUnits
import Utils.JSON

#ifdef javascript_HOST_ARCH
import Data.Time.LocalTime (minutesToTimeZone)
#endif

handleGetCurrentTimeZone :: IO TimeZone
#ifndef javascript_HOST_ARCH
handleGetCurrentTimeZone = getCurrentTimeZone
#endif
#ifdef javascript_HOST_ARCH
handleGetCurrentTimeZone = minutesToTimeZone <$> getTimezoneOffset

foreign import javascript "(() => new Date().getTimezoneOffset())"
  getTimezoneOffset :: IO Int
#endif

data IOAction

instance ConcurrentIO IOAction where
  data ConcurrentIOReq IOAction a where
    Sleep :: Time Double -> ConcurrentIOReq IOAction ()
    GetCurrentTime :: ConcurrentIOReq IOAction UTCTime
    GetCurrentTimeZone :: ConcurrentIOReq IOAction TimeZone
    GetUVIndexDataURI :: JSVal -> ConcurrentIOReq IOAction URI

  performIO = \case
    Sleep n -> threadDelay . floor $ n /~ micro second
    GetCurrentTime -> getCurrentTime
    GetCurrentTimeZone ->
      -- TEMP FIXME:  getCurrentTimeZone support JS with time-1.15, remove this when we upgraded
      handleGetCurrentTimeZone
    GetUVIndexDataURI v ->
      runMapLibre $
        getUVIndexDataURI v >>= \case
          Left err -> liftIO $ throwIO err
          Right r -> pure r

deriving instance Eq (ConcurrentIOReq IOAction a)

deriving instance Show (ConcurrentIOReq IOAction a)

instance ShowP (ConcurrentIOReq IOAction) where showp = show

instance Hashable (ConcurrentIOReq IOAction a) where
  hashWithSalt s =
    hashWithSalt @Int s . \case
      Sleep n -> 0 `hashWithSalt` (n /~ micro second)
      GetCurrentTime -> 1
      GetCurrentTimeZone -> 2
      GetUVIndexDataURI v -> 3 `hashWithSalt` v
