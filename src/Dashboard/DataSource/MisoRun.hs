{-# LANGUAGE TypeFamilies #-}

module Dashboard.DataSource.MisoRun (misoRunAction, misoRunJSM, misoRunJSMAction, State (..)) where

import Control.Monad.IO.Class
import Data.Foldable
import Data.Hashable
import Data.Text hiding (foldl', show)
import Data.Typeable
import Haxl.Core
import Language.Javascript.JSaddle
import Miso

-------------------------
-- NOTE: MisoRunAction --
-------------------------

data MisoRunAction action a where
  MisoRunAction :: action -> MisoRunAction action ()

instance (Eq action) => Eq (MisoRunAction action a) where
  (==) (MisoRunAction action1) (MisoRunAction action2) = action1 == action2

instance (Show action) => Show (MisoRunAction action a) where
  show (MisoRunAction action) = "MisoRunAction " <> show action

instance (Show action) => ShowP (MisoRunAction action) where showp = show

-- HACK: we don't care about the Hashable instance here, because we won't cache the result with `misoRunAction`
instance (Eq action) => Hashable (MisoRunAction action a) where
  hashWithSalt s _ = hashWithSalt s (1 :: Int)

instance (Typeable action) => DataSourceName (MisoRunAction action) where
  dataSourceName _ = pack "MisoRunAction"

instance (Typeable action) => StateKey (MisoRunAction action) where
  data State (MisoRunAction action) = MisoRunActionState JSContextRef (Sink action)

instance (Typeable action, Show action, Eq action) => DataSource u (MisoRunAction action) where
  fetch (MisoRunActionState jscontext sink) _ _ =
    SyncFetch $ \reqs ->
      flip runJSM jscontext $
        foldlM
          ( \() (BlockedFetch (MisoRunAction action) r) -> do
              sink action
              liftIO (putResult r $ Right ())
          )
          ()
          reqs

----------------------
-- NOTE: MisoRunJSM --
----------------------

data MisoRunJSM a where
  MisoRunJSM :: JSM () -> MisoRunJSM ()

instance Eq (MisoRunJSM a) where
  (==) _ _ = False

instance Show (MisoRunJSM a) where
  show (MisoRunJSM _) = "MisoRunJSM (some JSM)"

instance ShowP MisoRunJSM where showp = show

-- HACK: we don't care about the Hashable instance here, because we won't cache the result with `misoRunAction`
instance Hashable (MisoRunJSM a) where
  hashWithSalt s _ = hashWithSalt s (1 :: Int)

instance DataSourceName MisoRunJSM where
  dataSourceName _ = pack "MisoRunActionJSM"

instance StateKey MisoRunJSM where
  newtype State MisoRunJSM = MisoRunJSMState JSContextRef

instance DataSource u MisoRunJSM where
  fetch reqState@(MisoRunJSMState jscontext) = backgroundFetchPar (\(MisoRunJSM jsm) -> Right <$> runJSM jsm jscontext) reqState

----------------------------
-- NOTE: MisoRunJSMAction --
----------------------------

data MisoRunJSMAction action a where
  MisoRunJSMAction :: JSM action -> MisoRunJSMAction action ()

instance (Eq action) => Eq (MisoRunJSMAction action a) where
  (==) _ _ = False

instance (Show action) => Show (MisoRunJSMAction action a) where
  show = \case
    MisoRunJSMAction _ -> "MisoRunJSMAction"

instance (Show action) => ShowP (MisoRunJSMAction action) where showp = show

-- HACK: we don't care about the Hashable instance here, because we won't cache the result with `misoRunJSMAction`
instance (Eq action) => Hashable (MisoRunJSMAction action a) where
  hashWithSalt s _ = hashWithSalt s (1 :: Int)

instance (Typeable action) => DataSourceName (MisoRunJSMAction action) where
  dataSourceName _ = pack "MisoRunJSMAction"

instance (Typeable action) => StateKey (MisoRunJSMAction action) where
  data State (MisoRunJSMAction action) = MisoRunJSMActionState JSContextRef (Sink action)

instance (Typeable action, Show action, Eq action) => DataSource u (MisoRunJSMAction action) where
  fetch reqState@(MisoRunJSMActionState jscontext sink) =
    backgroundFetchPar (\(MisoRunJSMAction jsm) -> Right <$> runJSM (jsm >>= sink) jscontext) reqState

-------------------
-- NOTE: helpers --
-------------------

misoRunAction :: (Typeable action, Show action, Eq action) => action -> GenHaxl JSContextRef w ()
misoRunAction = uncachedRequest . MisoRunAction

misoRunJSM :: JSM () -> GenHaxl JSContextRef w ()
misoRunJSM = uncachedRequest . MisoRunJSM

misoRunJSMAction :: (Typeable action, Show action, Eq action) => JSM action -> GenHaxl JSContextRef w ()
misoRunJSMAction = uncachedRequest . MisoRunJSMAction
