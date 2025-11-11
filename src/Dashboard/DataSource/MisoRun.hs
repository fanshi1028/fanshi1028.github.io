{-# LANGUAGE TypeFamilies #-}

module Dashboard.DataSource.MisoRun (misoRunAction, State (..)) where

import Control.Monad.IO.Class
import Data.Foldable
import Data.Hashable
import Data.Text hiding (foldl', show)
import Data.Typeable
import Haxl.Core
import Language.Javascript.JSaddle
import Miso

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
      runJSaddle jscontext $
        foldlM
          ( \() (BlockedFetch (MisoRunAction action) r) -> do
              sink action
              liftIO (putResult r $ Right ())
          )
          ()
          reqs

misoRunAction :: (Typeable action, Show action, Eq action) => action -> GenHaxl JSContextRef w ()
misoRunAction = uncachedRequest . MisoRunAction
