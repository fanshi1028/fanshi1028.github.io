{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module App.Types where

import GHC.Generics
import Miso
import Miso.Router

newtype ToggleWASM = ToggleWASM Route

data Route
  = Index -- NOTE: Index must be the first one, code made assumpation base on its Enum being the first one.
  | Pomodoro
  | Dashboard
  deriving stock (Eq, Show, Enum, Bounded, Generic)
#ifndef wasm32_HOST_ARCH
  deriving anyclass (Router)

instance Router ToggleWASM where
  routeParser =
    routes
      [ path (ms "wasm") *> (ToggleWASM . to <$> gRouteParser),
        ToggleWASM Index <$ path (ms "wasm")
      ]
  fromRoute = \case
    ToggleWASM Index -> [ toPath $ ms "wasm" ]
    ToggleWASM route' -> toPath (ms "wasm") : gFromRoute (from route')
#endif

#ifdef wasm32_HOST_ARCH
instance Router Route where
  routeParser =
    routes
      [ path (ms "wasm") *> (to <$> gRouteParser),
        Index <$ path (ms "wasm")
      ]
  fromRoute = \case
    Index -> [ toPath $ ms "wasm" ]
    route' -> toPath (ms "wasm") : gFromRoute (from route')

instance Router ToggleWASM where
  routeParser = ToggleWASM . to <$> gRouteParser
  fromRoute (ToggleWASM route') = gFromRoute $ from route'
#endif

data Action
  = SetRoutingError RoutingError
  | GotoRoute Route
  | SetURI Route
  | SetPRDOpen Bool
  | AfterLoaded

data Model
  = RoutingError RoutingError
  | Model
      { _route :: Route,
        _loading :: Bool
      }
  deriving (Eq)
