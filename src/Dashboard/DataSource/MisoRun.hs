{-# LANGUAGE TypeFamilies #-}

module Dashboard.DataSource.MisoRun (misoRunAction, misoRunJSM, State (..)) where

import Data.Hashable
import Data.Text hiding (show)
import Data.Typeable
import Haxl.Core
import Language.Javascript.JSaddle
import Miso

data MisoRun action a where
  MisoRunAction :: action -> MisoRun action ()
  MisoRunJSM :: Proxy action -> JSM () -> MisoRun action ()

instance (Eq action) => Eq (MisoRun action a) where
  (==) (MisoRunAction action1) (MisoRunAction action2) = action1 == action2
  (==) _ _ = False

instance (Show action) => Show (MisoRun action a) where
  show = \case
    MisoRunAction action -> "MisoRunAction " <> show action
    MisoRunJSM _ _ -> "MisoRunJSM (some JSM)"

instance (Show action) => ShowP (MisoRun action) where
  showp misoRun = case misoRun of
    MisoRunAction _ -> show misoRun
    MisoRunJSM _ _ -> show misoRun

-- HACK: we don't care about the Hashable instance here, because we won't cache the result with `misoRunAction`
instance (Eq action) => Hashable (MisoRun action a) where
  hashWithSalt s _ = hashWithSalt s (1 :: Int)

instance (Typeable action) => DataSourceName (MisoRun action) where
  dataSourceName proxy = pack $ showsTypeRep (typeRep proxy) "(MisoRunAction)"

instance (Typeable action) => StateKey (MisoRun action) where
  data State (MisoRun action) = MisoRunActionState JSContextRef (Sink action)

instance (Typeable action, Show action, Eq action) => DataSource u (MisoRun action) where
  fetch reqState@(MisoRunActionState jscontext sink) =
    backgroundFetchPar
      ( fmap Right . flip runJSM jscontext . \case
          MisoRunAction action -> sink action
          MisoRunJSM _ jsm -> jsm
      )
      reqState

misoRunAction :: (Typeable action, Show action, Eq action) => action -> GenHaxl JSContextRef w ()
misoRunAction = uncachedRequest . MisoRunAction

misoRunJSM :: (Typeable action, Show action, Eq action) => Proxy action -> JSM () -> GenHaxl JSContextRef w ()
misoRunJSM proxy = uncachedRequest . MisoRunJSM proxy
