{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Dashboard.UVIndex where

import Control.Lens.Setter
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Foldable
import Data.Function
import Data.Hashable
import Data.List.NonEmpty hiding (groupBy, unzip)
import Data.Map.Strict as Map
import Data.Text hiding (null, show)
import Data.Time
import Data.Typeable
import Haxl.Core
import Language.Javascript.JSaddle
import Miso hiding (URI, on)
import Miso.FFI qualified as FFI
import Miso.Navigator
import Network.URI
import Network.URI.Lens

newtype UVIndex = UVIndex Double
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON)

data UVData = UVData
  { _time :: UTCTime,
    _uvi :: UVIndex
  } deriving stock Show

instance FromJSON UVData where
  parseJSON = withObject "UVdata" $ \o -> UVData <$> o .: "time" <*> o .: "uvi"

newtype UVIndexFetchError = UVIndexFetchError {_unUVIndexFetchError :: MisoString} deriving (Eq)

data UVIndexReq a where
  GetUVIndex :: Geolocation -> UVIndexReq ([UVData], NonEmpty UVData)
  deriving (Typeable)

deriving instance Eq (UVIndexReq a)

instance Hashable (UVIndexReq a) where
  hashWithSalt s (GetUVIndex (Geolocation lat long acc)) = hashWithSalt s (lat, long, acc) -- TEMP FIXME

deriving instance Show (UVIndexReq a)

instance ShowP UVIndexReq where showp = show

instance StateKey UVIndexReq where
  data State UVIndexReq = UVIndexReqState

instance DataSourceName UVIndexReq where
  dataSourceName _ = "currentuvindex.com"

uvIndexURI :: Geolocation -> URI
uvIndexURI (Geolocation lat long _) =
  URI
    "https:"
    (Just $ nullURIAuth & uriRegNameLens .~ unpack (dataSourceName @UVIndexReq Proxy))
    "/api/v1/uvi"
    ("?latitude=" <> show lat <> "&longitude=" <> show long)
    ""

instance DataSource u UVIndexReq where
  fetch _ _ _ = SyncFetch $
   \reqs -> do
     let 
        -- m :: Map.Map Geolocation [ResultVar ([UVData], NonEmpty UVData)]
        m = [(geo, [r]) | BlockedFetch (GetUVIndex geo) r <- reqs]
     unless (Map.null m) $
      void . liftJSM $
        traverseWithKey
          ( \geo@(Geolocation lat long _) (results :: [ResultVar ([UVData], NonEmpty UVData)]) ->
              -- NOTE: https://currentuvindex.com/api
              let parseAPIResponse = withObject "response" $ \o ->
                    o .: "ok" >>= \case
                      False -> Left <$> o .: "message"
                      True -> do
                        geo' <- (,) <$> o .: "latitude" <*> o .: "longitude"
                        now' <- o .: "now"
                        forecast <- o .: "forecast"
                        history <- o .: "history"
                        pure $ Right (geo', (history, now' :| forecast))
                  sucessCB = \(Response _ _ _ (v :: Value)) ->
                    liftIO . for_ results $ case iparse parseAPIResponse v of
                      ISuccess (Left msg) -> flip putFailure (Right $ UVIndexFetchError msg)
                      ISuccess (Right (geo', uvData@(_ :: [UVData], (UVData _ _) :| _))) -- TEMP FIXME: ignore history & forecast for now
                        | geo' == (lat, long) -> flip putSuccess uvData 
                        | otherwise -> flip putFailure (Right $ UVIndexFetchError "geolocation not matched")
                      IError path' err -> flip putFailure (Right . UVIndexFetchError . ms $ formatError path' err)
                  failCB = \(Response _ _ _ v) -> liftIO $ for_  results (`putFailure` (Right . UVIndexFetchError $ body v))
               in FFI.fetch (ms $ uriToString id (uvIndexURI geo) "") "GET" Nothing [] sucessCB failCB JSON
          )
          m
