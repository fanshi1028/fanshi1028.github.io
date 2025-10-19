{-# LANGUAGE TypeFamilies #-}

module Dashboard.DataSource.MisoRunAction (misoRunAction, State (..)) where

import Data.Hashable
import Data.Text hiding (show)
import Data.Typeable
import Haxl.Core
import Language.Javascript.JSaddle
import Miso

data MisoRunAction action a where
  MisoRunAction :: action -> MisoRunAction action ()

deriving instance (Eq action) => Eq (MisoRunAction action a)

deriving instance (Show action) => Show (MisoRunAction action a)

instance (Show action) => ShowP (MisoRunAction action) where showp = show

-- HACK: we don't care about the Hashable instance here, because we won't cache the result with `misoRunAction`
instance (Eq action) => Hashable (MisoRunAction action a) where
  hashWithSalt s _ = hashWithSalt s (1 :: Int)

instance (Typeable action) => DataSourceName (MisoRunAction action) where
  dataSourceName proxy = pack $ showsTypeRep (typeRep proxy) "(MisoRunAction)"

instance (Typeable action) => StateKey (MisoRunAction action) where
  data State (MisoRunAction action) = MisoRunActionState (Sink action)

instance (Typeable action, Show action, Eq action) => DataSource JSContextRef (MisoRunAction action) where
  fetch reqState@(MisoRunActionState sink) flags javaScriptContext =
    backgroundFetchPar
      (\(MisoRunAction action) -> runJSM (Right <$> sink action) javaScriptContext)
      reqState
      flags
      javaScriptContext

misoRunAction :: (Typeable action, Show action, Eq action) => action -> GenHaxl JSContextRef w ()
misoRunAction = uncachedRequest . MisoRunAction
