{-# LANGUAGE TypeFamilies #-}

module Dashboard.DataSource.BrowserGeolocationAPI where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Data.Hashable
import Data.Text (pack)
import Haxl.Core
import Language.Javascript.JSaddle
import Miso hiding ((<#))
import Miso.Navigator hiding (geolocation)
import UnliftIO.Exception

data LocationReq a where
  GetCurrentPosition :: LocationReq Geolocation

deriving instance Eq (LocationReq a)

instance Hashable (LocationReq a) where
  hashWithSalt s _ = hashWithSalt @Int s 1

deriving instance Show (LocationReq a)

instance ShowP LocationReq where showp = show

instance StateKey LocationReq where
  newtype State LocationReq = LocationReqState JSContextRef

instance DataSourceName LocationReq where
  dataSourceName _ = pack "browser geolocation api"

instance DataSource u LocationReq where
  fetch state@(LocationReqState jscontext) =
    asyncFetchAcquireRelease
      newEmptyMVar
      (const $ pure ())
      ( \resultMVar -> flip runJSM jscontext $ do
          options <- create
          (options <# "enableHighAccuracy") jsTrue
          successCB <-
            asyncCallback1 $ \v -> do
              result <-
                fromJSVal v >>= \case
                  Nothing ->
                    (jsg "JSON" # "stringify" $ [v])
                      >>= fromJSValUnchecked
                      <&> \stringified -> Left . toException . JSONError $ pack "Impossible! succeeded but failed to be parsed as Geolocation: " <> stringified
                  Just r -> pure $ Right r
              liftIO $ putMVar resultMVar result
          failCB <-
            asyncCallback1 $ \v -> do
              result <-
                fromJSVal @GeolocationError v >>= \case
                  Nothing ->
                    (jsg "JSON" # "stringify" $ [v])
                      >>= fromJSValUnchecked
                      <&> \stringified ->
                        Left . toException . JSONError $ pack "Impossible! failed with unexpected error: " <> stringified
                  Just err -> pure . Left . toException . FetchError . pack $ show err
              liftIO $ putMVar resultMVar result
          void $ jsg "navigator" ! "geolocation" # "getCurrentPosition" $ (successCB, failCB, options)
      )
      (const $ pure ())
      (\resultMVar GetCurrentPosition -> pure $ readMVar resultMVar)
      state
