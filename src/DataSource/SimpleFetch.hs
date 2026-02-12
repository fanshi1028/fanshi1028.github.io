{-# LANGUAGE TypeFamilies #-}

module DataSource.SimpleFetch where

import Data.Csv hiding (decode, encode)
import Data.Hashable
import Data.Text qualified as T
import Data.Typeable
import Data.Vector (Vector)
import DataSource.LocalStorage
import Haxl.Core
import Miso.JSON
import Miso.String
import Network.URI
import Utils.Haxl
import Prelude hiding ((+))

data SimpleFetch a where
  FetchJSON :: forall a. (FromJSON a, Typeable a) => URI -> SimpleFetch a
  FetchText :: URI -> SimpleFetch MisoString
  FetchCSV :: forall a. (FromRecord a) => Bool -> URI -> SimpleFetch (Vector a)

deriving instance Eq (SimpleFetch a)

deriving instance Show (SimpleFetch a)

instance ShowP SimpleFetch where showp = show

instance StateKey SimpleFetch where
  data State SimpleFetch = JSMActionState

instance Hashable (SimpleFetch a) where
  hashWithSalt s =
    hashWithSalt @Int s . \case
      FetchJSON uri' -> s `hashWithSalt` (0 :: Int) `hashWithSalt` uriToString id uri' ""
      FetchText uri' -> s `hashWithSalt` (1 :: Int) `hashWithSalt` uriToString id uri' ""
      FetchCSV hasHeader uri' ->
        s
          `hashWithSalt` (2 :: Int)
          `hashWithSalt` hasHeader
          `hashWithSalt` uriToString id uri' ""

instance DataSourceName SimpleFetch where
  dataSourceName _ = T.show . typeRepTyCon . typeRep $ Proxy @SimpleFetch

instance DataSource u SimpleFetch where
  fetch = backgroundFetchPar $ \case
    FetchJSON uri' -> fetchGetJSON Proxy uri'
    FetchText uri' -> fetchGetText uri'
    FetchCSV hasHeader uri' -> fetchGetCSV Proxy (if hasHeader then HasHeader else NoHeader) uri'
