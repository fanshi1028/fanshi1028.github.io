{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Utils.JSON where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Functor
import Data.Interval
import Data.List.NonEmpty
import Data.Scientific
import Data.Text (toLower)
import Data.Time
import Data.Vector qualified as V
import Miso.Aeson
import Miso.DSL hiding (Object)
import Miso.JSON
import Miso.String (fromMisoString, ms)
import Numeric.Natural
import Numeric.Units.Dimensional
import System.IO.Unsafe

instance FromJSON DayOfWeek where
  parseJSON = withText "DayOfWeek" $ \t -> case toLower $ fromMisoString t of
    "monday" -> pure Monday
    "tuesday" -> pure Tuesday
    "wednesday" -> pure Wednesday
    "thursday" -> pure Thursday
    "friday" -> pure Friday
    "saturday" -> pure Saturday
    "sunday" -> pure Sunday
    _ -> typeMismatch "DayOfWeek" $ String t

instance ToJSVal DayOfWeek where
  toJSVal = toJSVal . fromEnum

instance FromJSVal DayOfWeek where
  fromJSVal v = fmap toEnum <$> fromJSVal v

instance ToJSVal Day where
  toJSVal = toJSVal . fromEnum

instance FromJSVal Day where
  fromJSVal v = fmap toEnum <$> fromJSVal v

deriving via (UnAesonised Scientific) instance FromJSON Scientific

instance ToJSVal Scientific where
  toJSVal = toJSVal_Value . aesonToJSON . Aeson.toJSON

instance FromJSVal Scientific where
  fromJSVal v = (>>= (Aeson.parseMaybe Aeson.parseJSON . jsonToAeson)) <$> fromJSVal_Value v

-- NOTE: HACK TEMP FIXME
instance Show JSVal where
  show = fromMisoString . unsafePerformIO . jsonStringify

instance (FromJSVal a) => FromJSVal (V.Vector a) where
  fromJSVal v = fmap V.fromList <$> fromJSVal v

instance (ToJSVal a) => ToJSVal (V.Vector a) where
  toJSVal = toJSVal . V.toList

instance (FromJSVal a) => FromJSVal (NonEmpty a) where
  fromJSVal v = (>>= nonEmpty) <$> fromJSVal v

instance (ToJSVal a) => ToJSVal (NonEmpty a) where
  toJSVal = toJSVal . toList

instance (FromJSON a) => FromJSON (NonEmpty a) where
  parseJSON v =
    nonEmpty <$> parseJSON v >>= \case
      Nothing -> typeMismatch "non empty list" v
      Just r -> pure r

instance ToJSVal Natural where
  toJSVal = toJSVal @Int . fromIntegral

instance FromJSVal Natural where
  fromJSVal v =
    fromJSVal v
      <&> ( >>=
              \case
                x
                  | x >= 0 -> Just $ toEnum x
                  | otherwise -> Nothing
          )

-- NOTE: HACK TEMP FIXME
instance FromJSON JSVal where
  parseJSON = pure . unsafePerformIO . toJSVal_Value

newtype Aesonised a = Aesonised a

instance (FromJSON a) => Aeson.FromJSON (Aesonised a) where
  parseJSON (aesonToJSON -> v) = case parseEither parseJSON v of
    Left err -> Aeson.parseFail $ fromMisoString err
    Right r -> pure $ Aesonised r

newtype UnAesonised a = UnAesonised a

instance (Aeson.FromJSON a) => FromJSON (UnAesonised a) where
  parseJSON v'@(jsonToAeson -> v) = case Aeson.parseEither Aeson.parseJSON v of
    Left err -> typeMismatch (ms err) v'
    Right r -> pure $ UnAesonised r

instance (ToJSVal a, Fractional a, KnownDimension d) => ToJSVal (Quantity d a) where
  toJSVal v = toJSVal $ v /~ siUnit

instance (FromJSVal a, Num a, KnownDimension d) => FromJSVal (Quantity d a) where
  fromJSVal v = (fmap (*~ siUnit)) <$> fromJSVal v

extendedToJSVal :: (ToJSVal a) => Extended a -> IO JSVal
extendedToJSVal v = do
  o <- create
  case v of
    Finite a -> setProp "value" a o
    NegInf -> setProp "inf" False o
    PosInf -> setProp "inf" True o
  toJSVal o

extendedFromJSVal :: (FromJSVal a) => JSVal -> IO (Maybe (Extended a))
extendedFromJSVal v = do
  value <- v ! "value"
  isNull value >>= \case
    True ->
      v ! "inf"
        >>= fromJSVal
        <&> fmap
          ( \case
              True -> PosInf
              False -> NegInf
          )
    False -> fmap Finite <$> fromJSVal value

boundaryToBool :: Boundary -> Bool
boundaryToBool = \case
  Open -> True
  Closed -> False

boundaryFromBool :: Bool -> Boundary
boundaryFromBool = \case
  True -> Open
  False -> Closed

instance (ToJSVal a) => ToJSVal (Interval a) where
  toJSVal v = do
    let (le, lb) = lowerBound' v
        (ue, ub) = upperBound' v
    lower <- create
    setProp "value" (extendedToJSVal le) lower
    setProp "open" (boundaryToBool lb) lower
    upper <- create
    setProp "value" (extendedToJSVal ue) upper
    setProp "open" (boundaryToBool ub) upper
    o <- create
    setProp "upper" upper o
    setProp "lower" lower o
    toJSVal o

instance (Ord a, FromJSVal a) => FromJSVal (Interval a) where
  fromJSVal v = do
    mLe <- v ! "lower" ! "value" >>= extendedFromJSVal
    mLo <- fmap boundaryFromBool <$> (v ! "lower" ! "open" >>= fromJSVal)
    let mLower = (,) <$> mLe <*> mLo
    mUe <- v ! "upper" ! "value" >>= extendedFromJSVal
    mUo <- fmap boundaryFromBool <$> (v ! "upper" ! "open" >>= fromJSVal)
    let mUpper = (,) <$> mUe <*> mUo
    pure $ interval <$> mLower <*> mUpper
