module Utils.Haxl where

import Control.Concurrent
import Control.Exception (Exception (toException), SomeException)
import Control.Monad.IO.Class
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as BSL (fromStrict)
import Data.Csv
import Data.Functor
import Data.Text hiding (concat, elem, foldl', foldr, reverse, show)
import Data.Text qualified as T
import Data.Text.Encoding as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Encoding as TL
import Data.Typeable
import Data.Vector
import Haxl.Core
import Haxl.Core.Monad
import Language.Javascript.JSaddle
import Miso hiding (Decoder, URI, consoleLog, defaultOptions, on)
import Miso.FFI qualified as FFI
import Network.HTTP.Types
import Network.URI

consoleLog :: JSContextRef -> MisoString -> GenHaxl u w ()
consoleLog jscontext = void . unsafeLiftIO . runJSaddle jscontext . (jsg "console" # "log")

consoleLog' :: (ToJSON v) => JSContextRef -> v -> GenHaxl u w ()
consoleLog' jscontext = consoleLog jscontext . ms . toLazyText . encodePrettyToTextBuilder

failedResponseToException :: Response Value -> SomeException
failedResponseToException = \case
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

fetchJSM :: (forall a. (FromJSVal a) => Proxy a -> [(MisoString, MisoString)] -> CONTENT_TYPE -> StdMethod -> URI -> JSM (Either SomeException a))
fetchJSM _ headers contentType' method req = do
  resultMVar <- liftIO newEmptyMVar
  let url = ms $ uriToString id req ""
      successCB = \(Response _ _ _ v) -> liftIO . putMVar resultMVar $ Right v
      failCB = liftIO . putMVar resultMVar . Left . failedResponseToException
  FFI.fetch url (ms $ renderStdMethod method) Nothing headers successCB failCB contentType'
  liftIO $ readMVar resultMVar

fetchGetJSON :: (FromJSVal a) => Proxy a -> URI -> JSM (Either SomeException a)
fetchGetJSON proxy = fetchJSM proxy [accept =: applicationJSON] JSON GET

fetchGetText :: URI -> JSM (Either SomeException StrictText)
fetchGetText = fetchJSM Proxy [accept =: textPlain] TEXT GET

fetchGetCSV :: (FromRecord a) => Proxy a -> HasHeader -> URI -> JSM (Either SomeException (Vector a))
fetchGetCSV _ hasHeader uri =
  fetchJSM Proxy [accept =: ms "text/csv"] TEXT GET uri <&> \case
    Left err -> Left err
    Right txt -> case decode hasHeader . BSL.fromStrict $ T.encodeUtf8 txt of
      Left err -> Left . toException . MonadFail $ pack err
      Right r -> Right r

fetchGetBlob :: URI -> JSM (Either SomeException Blob)
fetchGetBlob = fetchJSM Proxy [accept =: ms "application/octect-stream"] BLOB GET

corsProxy :: URI -> URI
corsProxy uri' =
  nullURI
    { uriScheme = "https:",
      uriAuthority = Just $ nullURIAuth {uriRegName = "api.allorigins.win"},
      uriPath = "/get",
      uriQuery = renderQueryTextToString [(pack "url", Just . pack $ uriToString id uri' "")]
    }

renderQueryTextToString :: QueryText -> String
renderQueryTextToString = unpack . toStrict . TL.decodeUtf8 . toLazyByteString . renderQueryText True
