module Utils.Haxl where

import Control.Concurrent
import Control.Exception (Exception (toException), SomeException)
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as BSL (fromStrict)
import Data.Csv
import Data.Functor
import Data.Text hiding (concat, elem, foldl', foldr, reverse, show)
import Data.Text qualified as T
import Data.Text.Encoding as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding as TL
import Data.Typeable
import Data.Vector hiding ((!))
import Haxl.Core hiding (fetch)
import Haxl.Core.Monad
import Miso hiding (Decoder, URI, consoleLog, defaultOptions, go, on)
import Miso.FFI qualified as FFI (consoleLog)
import Miso.JSON hiding (decode)
import Network.HTTP.Types hiding (Header)
import Network.URI

consoleLog :: MisoString -> GenHaxl u w ()
consoleLog = unsafeLiftIO . FFI.consoleLog

consoleLog' :: (ToJSON v) => v -> GenHaxl u w ()
consoleLog' = consoleLog . encodePretty

failedResponseToException :: Response MisoString -> SomeException
failedResponseToException = \case
  Response Nothing headers mErrMsg _ -> toException . FetchError $ pack "CORS or Network Error" <> intercalate (pack ", ") [T.show headers, T.show mErrMsg]
  Response (Just code) headers mErrMsg err
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
        Nothing -> [T.show code, T.show headers, fromMisoString err]
        Just msg -> [T.show code, T.show msg, T.show headers, fromMisoString err]

fetchIO :: (forall a. (FromJSVal a, Typeable a) => Proxy a -> [(MisoString, MisoString)] -> CONTENT_TYPE -> StdMethod -> URI -> IO (Either SomeException a))
fetchIO proxy headers contentType' method req = do
  resultMVar <- newEmptyMVar
  let url = ms $ uriToString id req ""
      successCB (Response _ _ _ v) = putMVar resultMVar $ Right v
      failCB res@(Response _ _ _ v) = do
        v' <- jsonStringify v
        putMVar resultMVar . Left . failedResponseToException $
          (ms (showsTypeRep (typeRep proxy) " expected but got ") <> v') <$ res
  fetch url (ms $ renderStdMethod method) Nothing headers successCB failCB contentType'
  readMVar resultMVar

fetchGetJSON :: (FromJSVal a, Typeable a) => Proxy a -> URI -> IO (Either SomeException a)
fetchGetJSON proxy = fetchIO proxy [accept =: applicationJSON] JSON GET

fetchGetText :: URI -> IO (Either SomeException StrictText)
fetchGetText = fetchIO Proxy [accept =: textPlain] TEXT GET

fetchGetCSV :: (FromRecord a) => Proxy a -> HasHeader -> URI -> IO (Either SomeException (Vector a))
fetchGetCSV _ hasHeader uri =
  fetchIO Proxy [accept =: ms "text/csv"] TEXT GET uri <&> \case
    Left err -> Left err
    Right txt -> case decode hasHeader . BSL.fromStrict $ T.encodeUtf8 txt of
      Left err -> Left . toException . MonadFail $ pack err
      Right r -> Right r

fetchGetCSVNamed :: (FromNamedRecord a) => Proxy a -> URI -> IO (Either SomeException (Header, Vector a))
fetchGetCSVNamed _ uri =
  fetchIO Proxy [accept =: ms "text/csv"] TEXT GET uri <&> \case
    Left err -> Left err
    Right txt -> case decodeByName . BSL.fromStrict $ T.encodeUtf8 txt of
      Left err -> Left . toException . MonadFail $ pack err
      Right r -> Right r

fetchGetBlob :: URI -> IO (Either SomeException Blob)
fetchGetBlob = fetchIO Proxy [accept =: ms "application/octect-stream"] BLOB GET

renderQueryTextToString :: QueryText -> String
renderQueryTextToString = unpack . toStrict . TL.decodeUtf8 . toLazyByteString . renderQueryText True
