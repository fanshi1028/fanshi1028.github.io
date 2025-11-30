{-# LANGUAGE TypeFamilies #-}

module DataSource.SimpleFetch where

import Control.Exception (SomeException)
import Data.Csv
import Data.Hashable
import Data.Text hiding (concat, elem, foldl', foldr, reverse, show)
import Data.Text qualified as T
import Data.Typeable
import Data.Vector
import Haxl.Core
import Language.Javascript.JSaddle
import Network.URI
import Utils.Haxl

data SimpleFetch a where
  FetchJSON :: forall a. (FromJSVal a) => URI -> SimpleFetch a
  FetchText :: URI -> SimpleFetch StrictText
  FetchCSV :: forall a. (FromRecord a) => Bool -> URI -> SimpleFetch (Vector a)

deriving instance Eq (SimpleFetch a)

deriving instance Show (SimpleFetch a)

instance ShowP SimpleFetch where showp = show

instance StateKey SimpleFetch where
  data State SimpleFetch = JSMActionState JSContextRef

instance Hashable (SimpleFetch a) where
  hashWithSalt s =
    hashWithSalt @Int s . \case
      FetchJSON uri -> s `hashWithSalt` (0 :: Int) `hashWithSalt` uriToString id uri ""
      FetchText uri -> s `hashWithSalt` (1 :: Int) `hashWithSalt` uriToString id uri ""
      FetchCSV hasHeader uri ->
        s
          `hashWithSalt` (2 :: Int)
          `hashWithSalt` hasHeader
          `hashWithSalt` uriToString id uri ""

instance DataSourceName SimpleFetch where
  dataSourceName _ = T.show . typeRepTyCon . typeRep $ Proxy @SimpleFetch

instance DataSource u SimpleFetch where
  fetch _state@(JSMActionState jsContext) = backgroundFetchPar (runJSaddle jsContext . performJSM) _state
    where
      performJSM :: SimpleFetch a -> JSM (Either SomeException a)
      performJSM = \case
        FetchJSON uri -> fetchGetJSON Proxy uri
        FetchText uri -> fetchGetText uri
        FetchCSV hasHeader uri -> fetchGetCSV Proxy (if hasHeader then HasHeader else NoHeader) uri
