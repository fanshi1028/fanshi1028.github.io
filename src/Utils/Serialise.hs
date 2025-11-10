{-# LANGUAGE ViewPatterns #-}

module Utils.Serialise where

import Codec.CBOR.JSON
import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Control.Applicative
import Data.Aeson hiding (Encoding, decode, encode)
import Data.Function
import Data.Functor
import Data.Interval
import Data.Maybe
import Data.Scientific
import Language.Javascript.JSaddle hiding (Object, Success)
import Prelude hiding ((+))

newtype SerialisableValue = SerialisableValue Value deriving newtype (Eq, Show, FromJSON)

instance FromJSVal SerialisableValue where
  fromJSVal v = fmap SerialisableValue <$> fromJSVal v

instance Serialise SerialisableValue where
  encode (SerialisableValue v) = encodeValue v
  decode = SerialisableValue <$> decodeValue False

encodeScientific :: Scientific -> Encoding
encodeScientific (normalize -> v) = encodeListLen 2 <> encode (coefficient v) <> encode (base10Exponent v)

decodeScientific :: Decoder s Scientific
decodeScientific =
  decodeListLen >>= \case
    2 -> scientific <$> decode <*> decode
    _ -> fail "invalid Scientic encoding"

encodeExtended :: (a -> Encoding) -> Extended a -> Encoding
encodeExtended encoder = \case
  NegInf -> encodeListLen 1 <> encodeWord 0
  Finite a -> encodeListLen 2 <> encodeWord 1 <> encoder a
  PosInf -> encodeListLen 1 <> encodeWord 3

decodeExtended :: Decoder s a -> Decoder s (Extended a)
decodeExtended decoder =
  (,) <$> decodeListLen <*> decodeWord >>= \case
    (1, 0) -> pure NegInf
    (1, 3) -> pure PosInf
    (2, 1) -> Finite <$> decoder
    _ -> fail "invalid Extended encoding"

encodeBoundary :: Boundary -> Encoding
encodeBoundary = \case
  Open -> encodeWord 0
  Closed -> encodeWord 1

decodeBoundary :: Decoder s Boundary
decodeBoundary =
  decodeWord >>= \case
    0 -> pure Open
    1 -> pure Closed
    _ -> fail "invalid Boundary encoding"

encodeInterval :: (a -> Encoding) -> Interval a -> Encoding
encodeInterval encoder interval' =
  let (lb, lbBoundary) = lowerBound' interval'
      (ub, ubBoundary) = upperBound' interval'
   in encodeListLen 5 <> encodeWord 0 <> encodeExtended encoder lb <> encodeBoundary lbBoundary <> encodeExtended encoder ub <> encodeBoundary ubBoundary

decodeInterval :: (Ord a) => Decoder s a -> Decoder s (Interval a)
decodeInterval decoder =
  (,) <$> decodeListLen <*> decodeWord >>= \case
    (5, 0) -> interval <$> ((,) <$> decodeExtended decoder <*> decodeBoundary) <*> ((,) <$> decodeExtended decoder <*> decodeBoundary)
    _ -> fail "invalid Interval encoding"

-- NOTE: ref defaultEncodeList
encodeListWith :: (a -> Encoding) -> [a] -> Encoding
encodeListWith _ [] = encodeListLen 0
encodeListWith encoder ls = encodeListLenIndef <> foldr (\x r -> encoder x <> r) encodeBreak ls

-- NOTE: ref defaultDecodeList
decodeListWith :: Decoder s a -> Decoder s [a]
decodeListWith decoder =
  decodeListLenOrIndef >>= \case
    Nothing -> decodeSequenceLenIndef (flip (:)) [] reverse decoder
    Just n -> decodeSequenceLenN (flip (:)) [] reverse n decoder
