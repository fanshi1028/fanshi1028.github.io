{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Route (Route (..), Action (GotoRoute, SetPRDOpen), Model (..), routerComponent, routeToPRD) where

import GHC.Generics
import Miso
import Miso.Lens
import Miso.Router
import ProductRequirementDocument
import ProductRequirementDocument.Home
import ProductRequirementDocument.Pomodoro

data Route = Index | Pomodoro
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (Router)

data Action
  = ServerError MisoString
  | GotoRoute Route
  | SetURI Route
  | SetPRDOpen Bool

data Model
  = RoutingError MisoString
  | Model
      { _currentRoute :: Route,
        _prdOpen :: Bool
      }
  deriving (Eq)

routeToPRD :: Route -> ProductRequirementDocument
routeToPRD = \case
  Index -> homePRD
  Pomodoro -> pomodoroPRD

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  ServerError err -> this .= RoutingError err
  GotoRoute uri -> do
    io_ . pushURI $ toURI uri
    issue $ SetURI uri
  SetURI Index -> this .= Model Index False
  SetURI uri ->
    this %= Model uri . \case
      RoutingError _ -> False
      Model _ open -> open
  SetPRDOpen open ->
    this %= \case
      err@(RoutingError _) -> err
      Model uri _ -> Model uri open

routerComponent :: (Model -> View Model Action) -> Model -> Component parent Model Action
routerComponent routerView uri =
  (component uri updateModel routerView)
    { subs =
        [ routerSub $ \case
            Left err -> ServerError . ms $ show err
            Right uri' -> SetURI uri'
        ]
    }
