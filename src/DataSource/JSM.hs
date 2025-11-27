{-# LANGUAGE TypeFamilies #-}

module DataSource.JSM where

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

data JSMAction a where
  FetchJSON :: forall a. (FromJSVal a) => URI -> JSMAction a
  FetchText :: URI -> JSMAction StrictText
  FetchCSV :: forall a. (FromRecord a) => Bool -> URI -> JSMAction (Vector a)

deriving instance Eq (JSMAction a)

deriving instance Show (JSMAction a)

instance ShowP JSMAction where showp = show

instance StateKey JSMAction where
  data State JSMAction = JSMActionState JSContextRef

instance Hashable (JSMAction a) where
  hashWithSalt s =
    hashWithSalt @Int s . \case
      FetchJSON uri -> s `hashWithSalt` (0 :: Int) `hashWithSalt` uriToString id uri ""
      FetchText uri -> s `hashWithSalt` (1 :: Int) `hashWithSalt` uriToString id uri ""
      FetchCSV hasHeader uri ->
        s
          `hashWithSalt` (2 :: Int)
          `hashWithSalt` hasHeader
          `hashWithSalt` uriToString id uri ""

instance DataSourceName JSMAction where
  dataSourceName _ = T.show . typeRepTyCon . typeRep $ Proxy @JSMAction

instance DataSource u JSMAction where
  fetch _state@(JSMActionState jsContext) = backgroundFetchPar (runJSaddle jsContext . performJSM) _state
    where
      performJSM :: JSMAction a -> JSM (Either SomeException a)
      performJSM = \case
        FetchJSON uri -> fetchGetJSON Proxy uri
        FetchText uri -> fetchGetText uri
        FetchCSV hasHeader uri -> fetchGetCSV Proxy (if hasHeader then HasHeader else NoHeader) uri
