{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module DataSource.LocalStorage where

import Codec.Serialise
import Control.Exception (Exception (displayException, toException), SomeException)
import Control.Monad
import Control.Monad.IO.Class
import Data.Base64.Types
import Data.ByteString.Base64
import Data.ByteString.Lazy hiding (pack, unpack)
import Data.Hashable
import Data.IORef
import Data.Text hiding (show)
import Data.Text.Encoding
import Data.Text.Encoding.Base64.Error
import Data.Typeable
import Data.Void
import Haxl.Core
import Haxl.Core.DataCache
import Haxl.Core.Fetch
import Haxl.Core.Monad
import Haxl.Prelude hiding (forM_)
import Language.Javascript.JSaddle
import Miso (ms)
import Miso.FFI (consoleLog)

showReqResultSerialised :: (ShowP r, Serialise a) => ShowReq r a
showReqResultSerialised = (showp, unpack . extractBase64 . encodeBase64 . toStrict . serialise)

dataFetchWithSerialise :: forall u r a w. (DataSource u r, Hashable (r a), Typeable (r a), ShowP r, Serialise a) => r a -> GenHaxl u w a
dataFetchWithSerialise = dataFetchWithShow showReqResultSerialised

cacheResultWithLocalStorage ::
  ( Eq (r a),
    ShowP r,
    Hashable (r a),
    Typeable (r a),
    Serialise a
  ) =>
  r a -> JSM (GenHaxl u w ())
cacheResultWithLocalStorage req =
  cacheResultWithLocalStorage' req >>= \case
    Left err -> pure () <$ consoleLog (ms . displayException $ err)
    Right r -> pure . void $ cacheResultWithShow showReqResultSerialised req (pure r)

cacheResultWithLocalStorage' :: (ShowP r, Serialise a) => r a -> JSM (Either SomeException a)
cacheResultWithLocalStorage' req = do
  localStorage <- jsg "window" ! "localStorage"
  let key = showp req
  v <- localStorage # "getItem" $ [pack key]
  valIsNull v >>= \case
    True -> pure . Left . toException . NotFound . pack $ key <> ": not found in localStorage"
    False -> do
      txt <- fromJSValUnchecked v
      pure $ case decodeBase64Untyped $ encodeUtf8 txt of
        Left err -> Left . logicErrorToException $ DecodeError @Void err
        Right r -> case deserialiseOrFail $ fromStrict r of
          Left err -> Left $ logicErrorToException err
          Right r' -> Right r'

saveCacheToLocalStorage :: HaxlDataCache u w -> JSM ()
saveCacheToLocalStorage cache = do
  cacheShown <- liftIO $ showCache cache $ \(DataCacheItem IVar {ivarRef = !ref} _) ->
    readIORef ref >>= \case
      IVarFull (Ok a _) -> return (Just (Right a))
      IVarFull (ThrowHaxl e _) -> return (Just (Left e))
      IVarFull (ThrowIO e) -> return (Just (Left e))
      IVarEmpty _ -> return Nothing
  forM_ cacheShown $ \(_, subCacheShown) ->
    forM_ subCacheShown $ \case
      (reqStr, Left err) -> consoleLog . ms $ reqStr <> ":" <> displayException err
      (reqStr, Right r) ->
        () <$ do
          localStorage <- jsg "window" ! "localStorage"
          localStorage # "setItem" $ (reqStr, r)

data LocalStorage req a where
  FetchFromLocalStorage :: forall req a. (Serialise a) => req a -> LocalStorage req a

deriving instance (Show (req a)) => Show (LocalStorage req a)

instance (ShowP req) => ShowP (LocalStorage req) where
  showp (FetchFromLocalStorage req) = "localStorage:" <> showp req

deriving instance (Eq (req a)) => Eq (LocalStorage req a)

instance (Hashable (req a)) => Hashable (LocalStorage req a) where
  hashWithSalt s (FetchFromLocalStorage req) = s `hashWithSalt` req

instance (Typeable req) => StateKey (LocalStorage req) where
  data State (LocalStorage req) = LocalStorageReqState JSContextRef

instance (DataSourceName req) => DataSourceName (LocalStorage req) where
  dataSourceName _ = pack "localStorage:" <> dataSourceName (Proxy @req)

instance (DataSource u req) => DataSource u (LocalStorage req) where
  fetch state@(LocalStorageReqState jscontext) =
    backgroundFetchPar
      (\(FetchFromLocalStorage req) -> runJSaddle jscontext $ cacheResultWithLocalStorage' req)
      state

fetchCacheable :: (Typeable a, Serialise a, Request req a, DataSource u req, DataSource u (LocalStorage req)) => req a -> GenHaxl u w a
fetchCacheable req = uncachedRequest (FetchFromLocalStorage req) `catchAny` dataFetchWithSerialise req
