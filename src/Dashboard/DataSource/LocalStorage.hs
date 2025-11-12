{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Dashboard.DataSource.LocalStorage where

import Codec.Serialise
import Data.Hashable
import Data.Typeable
import Haxl.Core
import Haxl.LocalStorage
import Haxl.Prelude
import Language.Javascript.JSaddle

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
  dataSourceName _ = "localStorage:" <> dataSourceName (Proxy @req)

instance (DataSource u req) => DataSource u (LocalStorage req) where
  fetch state@(LocalStorageReqState jscontext) =
    backgroundFetchPar
      (\(FetchFromLocalStorage req) -> runJSaddle jscontext $ cacheResultWithLocalStorage' req)
      state

fetchCacheable :: (Typeable a, Serialise a, Request req a, DataSource u req, DataSource u (LocalStorage req)) => req a -> GenHaxl u w a
fetchCacheable req = uncachedRequest (FetchFromLocalStorage req) `catchAny` dataFetchWithSerialise req
