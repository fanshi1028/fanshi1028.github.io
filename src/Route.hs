{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Route (Route (..), ToggleWASM (..), Action (GotoRoute, SetPRDOpen, AfterLoaded), Model (..), routerComponent, routeToPRD) where

import Control.Monad
import GHC.Generics
import Language.Javascript.JSaddle
import Miso
import Miso.Lens
import Miso.Router
import ProductRequirementDocument
import ProductRequirementDocument.Dashboard
import ProductRequirementDocument.Home
import ProductRequirementDocument.Pomodoro

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

routeToPRD :: Route -> ProductRequirementDocument
routeToPRD = \case
  Index -> homePRD
  Pomodoro -> pomodoroPRD
  Dashboard -> dashboardPRD

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  SetRoutingError err -> this .= RoutingError err
  GotoRoute uri -> do
    io_ . pushURI $ toURI uri
    issue $ SetURI uri
  SetURI uri -> this .= Model uri True
  SetPRDOpen setOpen -> io_ . void $ do
    prdDialgoue <- getElementById prdDialogueId
    prdDialgoue # (if setOpen then "showModal" else "close") $ ()
  AfterLoaded ->
    get >>= \case
      RoutingError err -> pure ()
      Model uri _ -> put $ Model uri False

routerComponent :: (Model -> View Model Action) -> Model -> Component parent Model Action
routerComponent routerView uri =
  (component uri updateModel routerView)
    { subs =
        [ routerSub $ \case
            Left err -> SetRoutingError err
            Right uri' -> SetURI uri'
        ]
    }
