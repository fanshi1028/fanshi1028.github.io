{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Dashboard.DataSource.LocalStorage where

import Codec.Serialise
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
import Haxl.Prelude (catchAny)
import Language.Javascript.JSaddle
import UnliftIO.Exception hiding (catchAny)

data LocalStorage a where
  GetLocalStorage :: forall item. (Serialise item) => Text -> LocalStorage item
  SetLocalStorage :: forall item. (Serialise item) => Text -> item -> LocalStorage ()
  RemoveLocalStorage :: Text -> LocalStorage ()

instance Eq (LocalStorage a) where
  (==) (RemoveLocalStorage key1) (RemoveLocalStorage key2) = key1 == key2
  (==) _ _ = False

instance Show (LocalStorage item) where
  show = \case
    GetLocalStorage key -> "GetLocalStorage " <> unpack key
    SetLocalStorage key _ -> "SetLocalStorage " <> unpack key
    RemoveLocalStorage key -> "RemoveLocalStorage " <> unpack key

instance ShowP LocalStorage where showp = show

instance StateKey LocalStorage where
  data State LocalStorage = LocalStorageReqState

instance DataSourceName LocalStorage where
  dataSourceName _ = pack "localStorage"

instance DataSource JSContextRef LocalStorage where
  fetch state flags jscontext =
    syncFetch
      ($ jsg "window" ! "localStorage")
      (const $ pure ())
      ( \localStorage req -> pure . flip runJSM jscontext $ case req of
          GetLocalStorage key -> do
            v <- localStorage # "getItem" $ [key]
            ghcjsPure (isNull v) >>= \case
              True -> pure . Left . toException . NotFound $ key <> pack ": not found in localStorage"
              False -> do
                txt <- fromJSValUnchecked v
                case decodeBase64Untyped $ encodeUtf8 txt of
                  Left err -> fail . displayException @(Base64Error Void) $ DecodeError err
                  Right r -> case deserialiseOrFail $ fromStrict r of
                    Left err -> fail $ displayException err
                    Right r' -> pure $ Right r'
          SetLocalStorage key item ->
            Right () <$ do
              localStorage # "setItem" $ (key, extractBase64 . encodeBase64 . toStrict $ serialise item)
          RemoveLocalStorage key -> Right () <$ (localStorage # "removeItem" $ [key])
      )
      state
      flags
      jscontext

-- HACK: we don't care about the Hashable instance here, because we won't cache the result
instance Hashable (LocalStorage a) where
  hashWithSalt s _ = hashWithSalt s (1 :: Int)

getLocalStorage :: (Eq item, Typeable item, Show item, Serialise item) => Text -> GenHaxl JSContextRef w item
getLocalStorage k = uncachedRequest $ GetLocalStorage k

setLocalStorage :: (Serialise item) => Text -> item -> GenHaxl JSContextRef w ()
setLocalStorage k v = uncachedRequest $ SetLocalStorage k v

removeLocalStorage :: Text -> GenHaxl JSContextRef w ()
removeLocalStorage k = uncachedRequest $ RemoveLocalStorage k

makeReqStorageKey :: (Typeable req, Show req) => req -> Text
makeReqStorageKey req@(typeRepTyCon . typeOf -> tyCon) = pack $ tyConName tyCon <> ":" <> show req

loadCacheFromLocalStorage :: (Serialise a, Typeable a, Eq a, Show a) => Text -> (a -> Bool) -> GenHaxl JSContextRef w a
loadCacheFromLocalStorage key validateCache =
  getLocalStorage key >>= \case
    res
      | validateCache res -> pure res
      | otherwise -> fail $ "LocalStorage Cache Invalidated: " <> unpack key

fromLocalStorageOrDatafetch :: (Eq a, Typeable a, Serialise a, Request req a, DataSource JSContextRef req) => req a -> (a -> Bool) -> GenHaxl JSContextRef w a
fromLocalStorageOrDatafetch req@(makeReqStorageKey -> key) validateCache =
  loadCacheFromLocalStorage key validateCache
    `catchAny` do
      r <- dataFetch req
      setLocalStorage key r
      pure r
