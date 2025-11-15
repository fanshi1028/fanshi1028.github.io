{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Route (Route (..), Action (GotoRoute, SetPRDOpen), Model (..), routerComponent, routeToPRD) where

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

data Route
  = Index -- NOTE: Index must be the first one, code made assumpation base on its Enum being the first one.
  | Pomodoro
  | Dashboard
  deriving stock (Eq, Show, Enum, Bounded, Generic)
#ifndef WASM
  deriving anyclass (Router)
#endif

#ifdef WASM
instance Router Route where
  routeParser = path (ms "wasm") *> (to <$> gRouteParser)
  fromRoute route' = toPath (ms "wasm") : gFromRoute (from route')
#endif

data Action
  = SetRoutingError RoutingError
  | GotoRoute Route
  | SetURI Route
  | SetPRDOpen Bool

data Model
  = RoutingError RoutingError
  | Model Route
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
  SetURI uri -> this .= Model uri
  SetPRDOpen setOpen -> io_ . void $ do
    prdDialgoue <- getElementById prdDialogueId
    prdDialgoue # (if setOpen then "showModal" else "close") $ ()

routerComponent :: (Model -> View Model Action) -> Model -> Component parent Model Action
routerComponent routerView uri =
  (component uri updateModel routerView)
    { subs =
        [ routerSub $ \case
            Left err -> SetRoutingError err
            Right uri' -> SetURI uri'
        ]
    }
