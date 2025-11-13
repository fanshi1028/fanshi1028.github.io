{-# LANGUAGE TypeFamilies #-}

module Dashboard.DataSource.JSM where

import Control.Exception (SomeException)
import Data.Aeson.Encode.Pretty
import Data.Hashable
import Data.Text hiding (concat, elem, foldl', foldr, reverse, show)
import Data.Text qualified as T
import Data.Text.Lazy.Builder
import Data.Typeable
import Haxl.Core
import Language.Javascript.JSaddle
import Miso hiding (Decoder, URI, defaultOptions, on)
import Network.URI
import Utils.Fetch
import Utils.Serialise

data JSMAction a where
  FetchURI :: URI -> JSMAction SerialisableValue -- NOTE: assume we always fetch in GET, other method don't makes much sense in Haxl context, right?
  ConsoleLog :: MisoString -> JSMAction ()
  ConsoleLog' :: SerialisableValue -> JSMAction ()

deriving instance Eq (JSMAction a)

deriving instance Show (JSMAction a)

instance ShowP JSMAction where showp = show

instance StateKey JSMAction where
  data State JSMAction = JSMActionState JSContextRef

instance Hashable (JSMAction a) where
  hashWithSalt s =
    hashWithSalt @Int s . \case
      FetchURI uri -> s `hashWithSalt` (0 :: Int) `hashWithSalt` uriToString id uri ""
      ConsoleLog str -> s `hashWithSalt` (1 :: Int) `hashWithSalt` fromMisoString @StrictText str
      ConsoleLog' v -> s `hashWithSalt` (2 :: Int) `hashWithSalt` v

instance DataSourceName JSMAction where
  dataSourceName _ = T.show . typeRepTyCon . typeRep $ Proxy @JSMAction

instance DataSource u JSMAction where
  fetch _state@(JSMActionState jsContext) = backgroundFetchPar (runJSaddle jsContext . performJSM) _state
    where
      performJSM :: JSMAction a -> JSM (Either SomeException a)
      performJSM = \case
        FetchURI uri -> fetchGetJSON Proxy uri
        ConsoleLog str -> Right <$> Miso.consoleLog str
        ConsoleLog' v -> Right <$> (Miso.consoleLog . ms . toLazyText $ encodePrettyToTextBuilder v)

consoleLog :: MisoString -> GenHaxl u w ()
consoleLog = uncachedRequest . ConsoleLog

consoleLog' :: SerialisableValue -> GenHaxl u w ()
consoleLog' = uncachedRequest . ConsoleLog'
