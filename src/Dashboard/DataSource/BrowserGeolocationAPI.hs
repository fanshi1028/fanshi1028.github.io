{-# LANGUAGE TypeFamilies #-}

module Dashboard.DataSource.BrowserGeolocationAPI where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Data.Hashable
import Data.Text (pack)
import Haxl.Core
import Language.Javascript.JSaddle
import Miso
import Miso.Navigator hiding (geolocation)
import UnliftIO.Exception

-- NOTE: Copied from Miso.FFI.Internal
geolocation :: (JSVal -> JSM ()) -> (JSVal -> JSM ()) -> JSM ()
geolocation successful errorful = do
  geo <- jsg "navigator" ! "geolocation"
  cb1 <- asyncCallback1 successful
  cb2 <- asyncCallback1 errorful
  void $ geo # "getCurrentPosition" $ (cb1, cb2)

data LocationReq a where
  LocationReq :: LocationReq Geolocation

deriving instance Eq (LocationReq a)

instance Hashable (LocationReq a) where
  hashWithSalt s _ = hashWithSalt @Int s 1

deriving instance Show (LocationReq a)

instance ShowP LocationReq where showp = show

instance StateKey LocationReq where
  data State LocationReq = LocationReqState

instance DataSourceName LocationReq where
  dataSourceName _ = pack "browser geolocation api"

instance DataSource JSContextRef LocationReq where
  fetch reqState flags javaScriptContext =
    backgroundFetchPar
      ( \req -> case req of
          LocationReq -> handler req
      )
      reqState
      flags
      javaScriptContext
    where
      handler :: (forall a. (FromJSVal a) => LocationReq a -> IO (Either SomeException a))
      handler LocationReq = do
        successMVar <- newEmptyMVar @Geolocation
        failMVar <- newEmptyMVar @GeolocationError
        runJSM
          ( geolocation
              (fromJSValUnchecked >=> liftIO . putMVar successMVar)
              (fromJSValUnchecked >=> liftIO . putMVar failMVar)
          )
          javaScriptContext
        race
          ( -- NOTE: sadly Geolocation doesn't has a Exception instance for some reason
            toException . userError . show <$> readMVar failMVar
          )
          (readMVar successMVar)
