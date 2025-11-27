{-# LANGUAGE TypeFamilies #-}

module Dashboard.DataSource.JSM where

import Control.Exception (SomeException)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Csv
import Data.Functor
import Data.Hashable
import Data.Text hiding (concat, elem, foldl', foldr, reverse, show)
import Data.Text qualified as T
import Data.Text.Lazy.Builder
import Data.Typeable
import Data.Vector hiding ((!))
import Haxl.Core
import Haxl.Core.Monad
import Language.Javascript.JSaddle
import Miso hiding (Decoder, URI, consoleLog, defaultOptions, on)
import Network.URI
import Utils.Fetch
import Utils.Fetch.CSV
import Utils.Serialise

data JSMAction a where
  FetchJSON :: URI -> JSMAction SerialisableValue
  FetchCSV :: forall a. (FromRecord a) => URI -> JSMAction (Vector a)
  FetchText :: URI -> JSMAction StrictText

deriving instance Eq (JSMAction a)

deriving instance Show (JSMAction a)

instance ShowP JSMAction where showp = show

instance StateKey JSMAction where
  data State JSMAction = JSMActionState JSContextRef

instance Hashable (JSMAction a) where
  hashWithSalt s =
    hashWithSalt @Int s . \case
      FetchJSON uri -> s `hashWithSalt` (0 :: Int) `hashWithSalt` uriToString id uri ""
      FetchCSV uri -> s `hashWithSalt` (1 :: Int) `hashWithSalt` uriToString id uri ""
      FetchText uri -> s `hashWithSalt` (2 :: Int) `hashWithSalt` uriToString id uri ""

instance DataSourceName JSMAction where
  dataSourceName _ = T.show . typeRepTyCon . typeRep $ Proxy @JSMAction

instance DataSource u JSMAction where
  fetch _state@(JSMActionState jsContext) = backgroundFetchPar (runJSaddle jsContext . performJSM) _state
    where
      performJSM :: JSMAction a -> JSM (Either SomeException a)
      performJSM = \case
        FetchJSON uri -> fetchGetJSON Proxy uri
        FetchCSV uri -> fetchGetCSV uri
        FetchText uri -> fetchGetText uri

consoleLog :: JSContextRef -> MisoString -> GenHaxl u w ()
consoleLog jscontext = void . unsafeLiftIO . runJSaddle jscontext . (jsg "console" # "log")

consoleLog' :: (ToJSON v) => JSContextRef -> v -> GenHaxl u w ()
consoleLog' jscontext = consoleLog jscontext . ms . toLazyText . encodePrettyToTextBuilder
