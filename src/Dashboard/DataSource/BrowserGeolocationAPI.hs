{-# LANGUAGE TypeFamilies #-}

module Dashboard.DataSource.BrowserGeolocationAPI where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor
import Data.Hashable
import Data.Text (pack)
import Haxl.Core
import Language.Javascript.JSaddle
import Miso
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
  fetch (LocationReqState jscontext) _ _ = AsyncFetch $ \reqs inner -> do
    let results = [r | BlockedFetch GetCurrentPosition r <- reqs]
    case results of
      [] -> pure ()
      _ : _ -> flip runJSM jscontext $ do
        successCB <-
          asyncCallback1 $ \v -> do
            result <-
              fromJSVal v >>= \case
                Nothing ->
                  (jsg "JSON" # "stringify" $ [v])
                    >>= fromJSVal
                    <&> Left . toException . JSONError . \case
                      Nothing -> pack "Impossible!succeeded but failed to be parsed as Geolocation."
                      Just stringified -> pack "Impossible! succeeded but failed to be parsed as Geolocation: " <> stringified
                Just r -> pure $ Right r
            liftIO $ for_ results (flip putResult result)
        failCB <-
          asyncCallback1 $ \v -> do
            result <-
              fromJSVal @GeolocationError v >>= \case
                Nothing ->
                  (jsg "JSON" # "stringify" $ [v])
                    >>= fromJSVal
                    <&> Left . toException . JSONError . \case
                      Nothing -> pack "Impossible! failed with unstringifiable error"
                      Just stringified -> pack "Impossible! failed with unexpected error: " <> stringified
                Just err -> pure . Left . toException . FetchError . pack $ show err
            liftIO $ for_ results (flip putResult result)
        void $ jsg "navigator" ! "geolocation" # "getCurrentPosition" $ (successCB, failCB)
    inner
