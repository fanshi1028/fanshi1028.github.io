{-# LANGUAGE ViewPatterns #-}

module Utils.JS where

import Control.Exception (throw)
import Data.Aeson.Types
import Data.Functor
import Data.Text qualified as T
import Data.Typeable
import Haxl.Core hiding (throw)
import Language.Javascript.JSaddle

{-# WARNING fromJSValViaValue "partial, throw error when JSON assumption is wrong" #-}
fromJSValViaValue :: (FromJSON a, Typeable a) => Proxy a -> JSVal -> JSM (Maybe a)
fromJSValViaValue (typeRepTyCon . typeRep -> tyCon) a =
  fromJSVal a <&> \mv ->
    ifromJSON <$> mv >>= \case
      ISuccess r -> Just r
      IError path' err ->
        -- HACK
        throw . JSONError . T.pack $
          concat
            [ "Error in ",
              tyConModule tyCon,
              "(",
              tyConName tyCon,
              formatRelativePath path',
              "):",
              err
            ]
