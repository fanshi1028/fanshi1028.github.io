{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module DataSource.IO (ConcurrentIOReq (..), JSMAction (JSMAction), getUVIndexDataUriJSMAction) where

import Component.Foreign.MapLibre
import Control.Concurrent
import Data.Hashable
import Data.Time
import Haxl.Core
import Haxl.DataSource.ConcurrentIO
import Language.Javascript.JSaddle
import Network.URI
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.SIUnits

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

data JSMAction a = JSMAction JSContextRef (JSM a)

instance Eq (JSMAction a) where
  (==) _ _ = False -- HACK

instance Show (JSMAction a) where
  show _ = "JSMAction" -- HACK

instance ConcurrentIO IOAction where
  data ConcurrentIOReq IOAction a where
    Sleep :: Time Double -> ConcurrentIOReq IOAction ()
    RunJSMAction :: JSMAction a -> ConcurrentIOReq IOAction a
    GetCurrentTime :: ConcurrentIOReq IOAction UTCTime
    GetCurrentTimeZone :: ConcurrentIOReq IOAction TimeZone

  performIO = \case
    Sleep n -> threadDelay . floor $ n /~ micro second
    RunJSMAction (JSMAction jscontext jsm) -> runJSaddle jscontext jsm
    GetCurrentTime -> getCurrentTime
    GetCurrentTimeZone ->
      -- TEMP FIXME:  getCurrentTimeZone support JS with time-1.15, remove this when we upgraded
      handleGetCurrentTimeZone

deriving instance Eq (ConcurrentIOReq IOAction a)

deriving instance Show (ConcurrentIOReq IOAction a)

instance ShowP (ConcurrentIOReq IOAction) where showp = show

instance Hashable (ConcurrentIOReq IOAction a) where
  hashWithSalt s =
    hashWithSalt @Int s . \case
      Sleep n -> 0 `hashWithSalt` (n /~ micro second)
      RunJSMAction _ -> 1 -- HACK
      GetCurrentTime -> 2
      GetCurrentTimeZone -> 3

getUVIndexDataUriJSMAction :: (ToJSVal geoJSON) => JSContextRef -> geoJSON -> GenHaxl u w URI
getUVIndexDataUriJSMAction jscontext geoJSON = do
  r <- uncachedRequest . RunJSMAction . JSMAction jscontext . runMapLibre $ getUVIndexDataURI geoJSON
  case r of
    Left err -> throw err
    Right uri' -> pure uri'
