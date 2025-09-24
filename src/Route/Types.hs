{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Route.Types where

import Data.Aeson hiding ((.=))
import GHC.Generics
import Miso
import Miso.Router

data Route = Index | Pomodoro
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (Router, ToJSON, FromJSON)

data Action
  = ServerError MisoString
  | PushURI Route
  | SetURI Route

type Model = Either MisoString Route
