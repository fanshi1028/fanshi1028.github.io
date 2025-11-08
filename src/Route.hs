{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

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
  deriving anyclass (Router)

data Action
  = ServerError MisoString
  | GotoRoute Route
  | SetURI Route
  | SetPRDOpen Bool

data Model
  = RoutingError MisoString
  | Model Route
  deriving (Eq)

routeToPRD :: Route -> ProductRequirementDocument
routeToPRD = \case
  Index -> homePRD
  Pomodoro -> pomodoroPRD
  Dashboard -> dashboardPRD

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  ServerError err -> this .= RoutingError err
  GotoRoute uri -> do
    io_ . consoleLog . ms $ "1: " <> show uri
    io_ . pushURI $ toURI uri
    io_ . consoleLog . ms $ "2: " <> show uri
    issue $ SetURI uri
    io_ . consoleLog . ms $ "3: " <> show uri
  SetURI uri -> do
    io_ . consoleLog . ms $ "4: " <> show uri
    this .= Model uri
    io_ . consoleLog . ms $ "5: " <> show uri
  SetPRDOpen setOpen -> io_ . void $ do
    prdDialgoue <- getElementById prdDialogueId
    prdDialgoue # (if setOpen then "showModal" else "close") $ ()

routerComponent :: (Model -> View Model Action) -> Model -> Component parent Model Action
routerComponent routerView uri =
  (component uri updateModel routerView)
    { subs =
        [ routerSub $ \case
            Left err -> ServerError . ms $ show err
            Right uri' -> SetURI uri'
        ]
    }
