{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module DataSource.BrowserGeolocationAPI where

import Control.Concurrent
import Control.Exception (Exception (toException))
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Data.Hashable
import Data.Text (pack)
import Haxl.Core
import Miso
import Miso.JSON
import Miso.Navigator hiding (geolocation)

data LocationReq a where
  GetCurrentPosition :: LocationReq Geolocation

deriving instance Eq (LocationReq a)

instance Hashable (LocationReq a) where
  hashWithSalt s _ = hashWithSalt @Int s 1

deriving instance Show (LocationReq a)

instance ShowP LocationReq where showp = show

instance StateKey LocationReq where
  data State LocationReq = LocationReqState

instance DataSourceName LocationReq where
  dataSourceName _ = "browser geolocation api"

instance DataSource u LocationReq where
  fetch =
    asyncFetchAcquireRelease
      newEmptyMVar
      (const $ pure ())
      ( \resultMVar -> do
          options <- create
          setProp "enableHighAccuracy" True options
          successCB <-
            syncCallback1 $ \v -> do
              result <-
                fromJSVal v >>= \case
                  Nothing ->
                    jsonStringify v <&> \stringified ->
                      Left . toException . JSONError . fromMisoString $ "Impossible! succeeded but failed to be parsed as Geolocation: " <> stringified
                  Just r -> pure $ Right r
              liftIO $ putMVar resultMVar result
          failCB <-
            syncCallback1 $ \v -> do
              result <-
                fromJSVal @GeolocationError v >>= \case
                  Nothing ->
                    jsonStringify v <&> \stringified ->
                      toException . JSONError . fromMisoString $ "Impossible! failed with unexpected error: " <> stringified
                  Just err -> pure . toException . FetchError . pack $ show err
              liftIO . putMVar resultMVar $ Left result
          void $ jsg "navigator" ! "geolocation" # "getCurrentPosition" $ (successCB, failCB, options)
      )
      (const $ pure ())
      (\resultMVar GetCurrentPosition -> pure $ readMVar resultMVar)
