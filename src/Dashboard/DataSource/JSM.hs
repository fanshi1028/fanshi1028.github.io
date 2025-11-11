{-# LANGUAGE TypeFamilies #-}

module Dashboard.DataSource.JSM where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Data.Aeson
import Data.Hashable
import Data.Text hiding (concat, elem, foldl', foldr, reverse, show)
import Data.Text qualified as T
import Data.Typeable
import Haxl.Core
import Language.Javascript.JSaddle
import Miso hiding (Decoder, URI, defaultOptions, on)
import Miso.FFI qualified as FFI
import Network.HTTP.Types
import Network.URI
import UnliftIO.Exception
import Utils.Serialise

data JSMAction a where
  FetchURI :: URI -> JSMAction SerialisableValue -- NOTE: assume we always fetch in GET, other method don't makes much sense in Haxl context, right?
  ConsoleLog :: MisoString -> JSMAction ()
  ConsoleLog' :: SerialisableValue -> JSMAction ()

deriving instance Eq (JSMAction a)

deriving instance Show (JSMAction a)

instance ShowP JSMAction where showp = show

instance StateKey JSMAction where
  data State JSMAction = JSMActionState JSContextRef

instance Hashable (JSMAction a) where
  hashWithSalt s =
    hashWithSalt @Int s . \case
      FetchURI uri -> s `hashWithSalt` (0 :: Int) `hashWithSalt` uriToString id uri ""
      ConsoleLog str -> s `hashWithSalt` (1 :: Int) `hashWithSalt` fromMisoString @StrictText str
      ConsoleLog' v -> s `hashWithSalt` (2 :: Int) `hashWithSalt` v

instance DataSourceName JSMAction where
  dataSourceName _ = T.show . typeRepTyCon . typeRep $ Proxy @JSMAction

instance DataSource u JSMAction where
  fetch _state@(JSMActionState jsContext) = backgroundFetchPar (runJSaddle jsContext . performJSM) _state
    where
      performJSM :: JSMAction a -> JSM (Either SomeException a)
      performJSM = \case
        FetchURI uri -> fetchGetJSM Proxy uri
        ConsoleLog str -> Right <$> consoleLog str
        ConsoleLog' v -> Right <$> ((jsg "JSON" # "stringify") [v] >>= consoleLog')

fetchJSM :: (forall a. (FromJSVal a) => Proxy a -> StdMethod -> URI -> JSM (Either SomeException a))
fetchJSM _ method req = do
  successMVar <- liftIO newEmptyMVar
  failMVar <- liftIO newEmptyMVar
  let url = ms $ uriToString id req ""
      successCB = \(Response _ _ _ v) -> liftIO $ putMVar successMVar v
      failCB =
        liftIO . putMVar failMVar . \case
          Response Nothing headers mErrMsg _ -> toException . FetchError $ pack "CORS or Network Error" <> intercalate (pack ", ") [T.show headers, T.show mErrMsg]
          Response (Just code) headers mErrMsg (v :: Value)
            | code == 400 -> toException . InvalidParameter $ pack "InvalidParameter: " <> errorDetails
            | code == 404 -> toException . NotFound $ pack "Not Found: " <> errorDetails
            | code == 408 -> toException . FetchError $ pack "TEMP FIXME Request Timeout: " <> errorDetails
            | code == 425 -> toException . FetchError $ pack "TEMP FIXME To Early: " <> errorDetails
            | code == 429 -> toException . FetchError $ pack "TEMP FIXME Too Many Requests: " <> errorDetails
            | code == 500 -> toException . FetchError $ pack "TEMP FIXME Internal Server Error: " <> errorDetails
            | code == 502 -> toException . FetchError $ pack "TEMP FIXME Bad Gateway: " <> errorDetails
            | code == 503 -> toException . FetchError $ pack "TEMP FIXME Service Unavailable: " <> errorDetails
            | code == 504 -> toException . FetchError $ pack "TEMP FIXME Gateway Timeout: " <> errorDetails
            | otherwise -> toException . MonadFail $ pack "Error: " <> errorDetails
            where
              errorDetails = intercalate (pack ", ") $ case mErrMsg of
                Nothing -> [T.show code, T.show headers, T.show v]
                Just msg -> [T.show code, T.show msg, T.show headers, T.show v]
  FFI.fetch url (ms $ renderStdMethod method) Nothing [] successCB failCB JSON
  liftIO $ race (readMVar failMVar) (readMVar successMVar)

fetchGetJSM :: (forall a. (FromJSVal a) => Proxy a -> URI -> JSM (Either SomeException a))
fetchGetJSM proxy = fetchJSM proxy GET
