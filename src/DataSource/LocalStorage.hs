{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module DataSource.LocalStorage where

import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Data.HashTable.IO qualified as H
import Data.Hashable
import Data.Text hiding (show)
import Data.Typeable
import Haxl.Core
import Haxl.Core.DataCache
import Haxl.Core.Fetch
import Haxl.Core.Monad
import Haxl.Prelude hiding (forM_)
import Miso (ms)
import Miso.DSL
import Miso.FFI
import Miso.JSON
import Miso.String (fromMisoString)
import System.IO.Unsafe
import UnliftIO hiding (catchAny)

showReqResultSerialised :: (ShowP r, ToJSVal a) => ShowReq r a
showReqResultSerialised = (showp, fromMisoString . unsafePerformIO . (jsonStringify <=< toJSVal))

dataFetchWithSerialise :: forall u r a w. (DataSource u r, Hashable (r a), Typeable (r a), ShowP r, ToJSVal a) => r a -> GenHaxl u w a
dataFetchWithSerialise = dataFetchWithShow showReqResultSerialised

cachedRequestWithLocalStorage :: (ShowP r, FromJSVal a) => r a -> IO (Either SomeException a)
cachedRequestWithLocalStorage (showp -> key) = do
  v <- (jsg "window" ! "localStorage") # "getItem" $ [pack key]
  isNull v >>= \case
    True -> pure . Left . toException . NotFound . pack $ key <> ": not found in localStorage"
    False ->
      fromJSVal v >>= \case
        Nothing -> do
          unexpected <- jsonStringify v
          pure . Left . internalErrorToException . UnexpectedType $ "impossible! LocalStorage " <> pack key <> ": returned non-string value " <> fromMisoString unexpected
        Just str ->
          jsonParse str
            >>= fromJSVal
            <&> \case
              Nothing -> Left . internalErrorToException . JSONError $ "impossible! LocalStorage " <> pack key <> ": fail to parse, " <> fromMisoString str
              Just r -> Right r

saveCacheToLocalStorage :: HaxlDataCache u w -> IO ()
saveCacheToLocalStorage (DataCache cache) = H.mapM_ goSubCache cache
  where
    goSubCache :: (TypeRep, SubCache (DataCacheItem u w)) -> IO ()
    goSubCache (_ty, SubCache showReq showRes hm) =
      H.mapM_
        ( \(showReq -> reqStr, (DataCacheItem IVar {ivarRef = !ref} _)) ->
            let logError err = consoleError $ ms $ reqStr <> ":" <> displayException err
             in liftIO (readIORef ref) >>= \case
                  IVarEmpty _ -> pure ()
                  IVarFull (Ok a _) -> void $ do
                    localStorage <- jsg "window" ! "localStorage"
                    localStorage # "setItem" $ (reqStr, showRes a)
                  IVarFull (ThrowHaxl e _) -> logError e
                  IVarFull (ThrowIO e) -> logError e
        )
        hm

data LocalStorage req a where
  FetchFromLocalStorage :: forall req a. (FromJSVal a) => req a -> LocalStorage req a

deriving instance (Show (req a)) => Show (LocalStorage req a)

instance (ShowP req) => ShowP (LocalStorage req) where
  showp (FetchFromLocalStorage req) = "localStorage:" <> showp req

deriving instance (Eq (req a)) => Eq (LocalStorage req a)

instance (Hashable (req a)) => Hashable (LocalStorage req a) where
  hashWithSalt s (FetchFromLocalStorage req) = s `hashWithSalt` req

instance (Typeable req) => StateKey (LocalStorage req) where
  data State (LocalStorage req) = LocalStorageReqState

instance (DataSourceName req) => DataSourceName (LocalStorage req) where
  dataSourceName _ = pack "localStorage:" <> dataSourceName (Proxy @req)

instance (DataSource u req) => DataSource u (LocalStorage req) where
  fetch = backgroundFetchPar $ \(FetchFromLocalStorage req) -> cachedRequestWithLocalStorage req

fetchCacheable :: (Typeable a, FromJSVal a, ToJSVal a, Request req a, DataSource u req, DataSource u (LocalStorage req)) => req a -> GenHaxl u w a
fetchCacheable req = uncachedRequest (FetchFromLocalStorage req) `catchAny` dataFetchWithSerialise req
