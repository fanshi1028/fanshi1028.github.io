{-# LANGUAGE ViewPatterns #-}

module Haxl.LocalStorage where

import Codec.Serialise
import Control.Monad
import Control.Monad.IO.Class
import Data.Base64.Types
import Data.ByteString.Base64
import Data.ByteString.Lazy hiding (pack, unpack)
import Data.Hashable
import Data.Text hiding (show)
import Data.Text.Encoding
import Data.Text.Encoding.Base64.Error
import Data.Typeable
import Data.Void
import Haxl.Core
import Haxl.Core.DataCache
import Haxl.Core.Fetch
import Haxl.Core.Monad
import Language.Javascript.JSaddle
import Miso (ms)
import Miso.FFI (consoleLog)
import UnliftIO.Exception
import UnliftIO.IORef

makeReqLocalStorageKey :: (Typeable req, Show req) => req -> String
makeReqLocalStorageKey req = show (typeOf req) <> ":" <> show req

showReqResultSerialised :: (Show (r a), Serialise a) => ShowReq r a
showReqResultSerialised = (show, unpack . extractBase64 . encodeBase64 . toStrict . serialise)

dataFetchWithSerialise :: forall u r a w. (DataSource u r, Eq (r a), Hashable (r a), Typeable (r a), Show (r a), Serialise a) => r a -> GenHaxl u w a
dataFetchWithSerialise = dataFetchWithShow showReqResultSerialised

cacheResultWithLocalStorage ::
  ( Eq (r a),
    Show (r a),
    Hashable (r a),
    Typeable (r a),
    Serialise a
  ) =>
  r a -> (a -> Bool) -> JSM (GenHaxl u w ())
cacheResultWithLocalStorage req validateCache = do
  localStorage <- jsg "window" ! "localStorage"
  let key = makeReqLocalStorageKey req
  v <- localStorage # "getItem" $ [pack key]
  eCacheReulst <-
    valIsNull v >>= \case
      True -> pure . Left $ key <> ": not found in localStorage"
      False -> do
        txt <- fromJSValUnchecked v
        pure $ case decodeBase64Untyped $ encodeUtf8 txt of
          Left err -> Left . displayException $ DecodeError @Void err
          Right r -> case deserialiseOrFail $ fromStrict r of
            Left err -> Left $ displayException err
            Right r' -> Right r'
  case eCacheReulst of
    Left err -> pure () <$ consoleLog (ms err)
    Right r
      | validateCache r -> pure . void $ cacheResultWithShow showReqResultSerialised req (pure r)
      | otherwise -> pure () <$ consoleLog (ms $ "LocalStorage Cache Invalidate: " <> key)

saveCacheToLocalStorage :: HaxlDataCache u w -> JSM ()
saveCacheToLocalStorage cache = do
  cacheShown <- liftIO $ showCache cache $ \(DataCacheItem IVar {ivarRef = !ref} _) ->
    readIORef ref >>= \case
      IVarFull (Ok a _) -> return (Just (Right a))
      IVarFull (ThrowHaxl e _) -> return (Just (Left e))
      IVarFull (ThrowIO e) -> return (Just (Left e))
      IVarEmpty _ -> return Nothing
  forM_ cacheShown $ \(showsTypeRep -> mkTypeRepStr, subCacheShown) ->
    forM_ subCacheShown $ \case
      (reqStr, Left err) -> consoleLog . ms $ reqStr <> ":" <> displayException err
      (reqStr, Right r) ->
        () <$ do
          localStorage <- jsg "window" ! "localStorage"
          localStorage # "setItem" $ ((mkTypeRepStr "" <> ":" <> reqStr), r)
