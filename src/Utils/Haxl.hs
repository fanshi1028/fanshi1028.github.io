module Utils.Haxl where

import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Data.Functor
import Data.Text.Lazy.Builder
import Data.Typeable
import Haxl.Core
import Haxl.Core.Monad
import Language.Javascript.JSaddle
import Miso hiding (consoleLog)

consoleLog :: JSContextRef -> MisoString -> GenHaxl u w ()
consoleLog jscontext = void . unsafeLiftIO . runJSaddle jscontext . (jsg "console" # "log")

consoleLog' :: (ToJSON v) => JSContextRef -> v -> GenHaxl u w ()
consoleLog' jscontext = consoleLog jscontext . ms . toLazyText . encodePrettyToTextBuilder

misoRunAction :: (Typeable action, Show action, Eq action) => JSContextRef -> (Sink action) -> action -> GenHaxl e w ()
misoRunAction jscontext sink = unsafeLiftIO . runJSaddle jscontext . sink
