module Dashboard.DataSource.MisoRun (misoRunAction) where

import Data.Typeable
import Haxl.Core
import Haxl.Core.Monad
import Language.Javascript.JSaddle
import Miso

misoRunAction :: (Typeable action, Show action, Eq action) => JSContextRef -> (Sink action) -> action -> GenHaxl e w ()
misoRunAction jscontext sink = unsafeLiftIO . runJSaddle jscontext . sink
