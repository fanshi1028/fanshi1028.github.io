module Utils.Haxl where

import Control.Concurrent
import Control.Exception (Exception (toException), SomeException)
import Control.Monad
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
import Data.Vector hiding (create, forM_, (!))
import Haxl.Core hiding (fetch)
import Haxl.Core.Monad
import Miso hiding (Decoder, URI, consoleLog, defaultOptions, fetch, go, on)
import Miso.FFI qualified as FFI (consoleLog)
import Miso.JSON hiding (Object, decode)
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

-- TEMP FIXME: copied from Miso but replaced fromJSValUnchecked with fromJSVal
fetch ::
  (FromJSON success, FromJSON error) =>
  -- | url
  MisoString ->
  -- | method
  MisoString ->
  -- | body
  Maybe JSVal ->
  -- | headers
  [(MisoString, MisoString)] ->
  -- | successful callback
  (Response success -> IO ()) ->
  -- | errorful callback
  (Either MisoString (Response error) -> IO ()) ->
  -- | content type
  CONTENT_TYPE ->
  IO ()
fetch url method maybeBody requestHeaders successful errorful type_ = do
  successful_ <-
    toJSVal
      =<< asyncCallback1
        ( \a ->
            fromJSVal a >>= \case
              Just res@(Response _ _ _ a') -> case parseEither parseJSON a' of
                Left err -> do
                  errorful . Left $ err <> ms " " <> ms (show a')
                Right a'' -> successful $ a'' <$ res
              Nothing -> do
                unexpected <- jsonStringify a
                errorful $ Left $ ms "impossible! Unexpected Response format: " <> unexpected
        )
  errorful_ <-
    toJSVal
      =<< asyncCallback1
        ( \a ->
            fromJSVal a >>= \case
              Just res@(Response _ _ _ a') -> case parseEither parseJSON a' of
                Left err -> errorful $ Left err
                Right a'' -> errorful . Right $ a'' <$ res
              Nothing -> do
                unexpected <- jsonStringify a
                errorful $ Left $ ms "impossible! Unexpected Response format: " <> unexpected
        )
  moduleMiso <- jsg $ ms "miso"
  url_ <- toJSVal url
  method_ <- toJSVal method
  body_ <- toJSVal maybeBody
  Object headers_ <- do
    o <- create
    forM_ requestHeaders $ \(k, v) -> set k v o
    pure o
  typ <- toJSVal type_
  void $
    moduleMiso # (ms "fetchCore") $
      [ url_,
        method_,
        body_,
        headers_,
        successful_,
        errorful_,
        typ
      ]

fetchIO :: (forall a. (FromJSON a, Typeable a) => Proxy a -> [(MisoString, MisoString)] -> CONTENT_TYPE -> StdMethod -> URI -> IO (Either SomeException a))
fetchIO proxy headers contentType' method req = do
  resultMVar <- newEmptyMVar
  let url = ms $ uriToString id req ""
      successCB (Response _ _ _ v) = putMVar resultMVar $ Right v
      failCB (Left err) =
        putMVar resultMVar . Left . logicBugToException . UnexpectedType . fromMisoString $
          (ms (showsTypeRep (typeRep proxy) " expected but got err instead: ") <> err)
      failCB (Right res@(Response _ _ _ v)) = do
        v' <- toJSVal_Value v >>= jsonStringify
        putMVar resultMVar . Left . failedResponseToException $
          (ms (showsTypeRep (typeRep proxy) " expected but got ") <> v') <$ res
  fetch url (ms $ renderStdMethod method) Nothing headers successCB failCB contentType'
  readMVar resultMVar

fetchGetJSON :: (FromJSON a, Typeable a) => Proxy a -> URI -> IO (Either SomeException a)
fetchGetJSON proxy = fetchIO proxy [accept =: applicationJSON] JSON GET

fetchGetText :: URI -> IO (Either SomeException MisoString)
fetchGetText = fetchIO Proxy [accept =: textPlain] TEXT GET

fetchGetCSV :: (FromRecord a) => Proxy a -> HasHeader -> URI -> IO (Either SomeException (Vector a))
fetchGetCSV _ hasHeader uri =
  fetchIO Proxy [accept =: ms "text/csv"] TEXT GET uri <&> \case
    Left err -> Left err
    Right txt -> case decode hasHeader . BSL.fromStrict $ T.encodeUtf8 $ fromMisoString txt of
      Left err -> Left . toException . MonadFail $ pack err
      Right r -> Right r

fetchGetCSVNamed :: (FromNamedRecord a) => Proxy a -> URI -> IO (Either SomeException (Header, Vector a))
fetchGetCSVNamed _ uri =
  fetchIO Proxy [accept =: ms "text/csv"] TEXT GET uri <&> \case
    Left err -> Left err
    Right txt -> case decodeByName . BSL.fromStrict . T.encodeUtf8 $ fromMisoString txt of
      Left err -> Left . toException . MonadFail $ pack err
      Right r -> Right r


renderQueryTextToString :: QueryText -> String
renderQueryTextToString = unpack . toStrict . TL.decodeUtf8 . toLazyByteString . renderQueryText True
