{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Route (Route (..), Action (GotoRoute, SetPRDOpen), Model (..), routerComponent) where

import GHC.Generics
import Miso
import Miso.Lens
import Miso.Router
import ProductRequirementDocument

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

updateModel :: (Route -> ProductRequirementDocument) -> Action -> Effect parent Model Action
updateModel routeToPRD = \case
  ServerError err -> this .= RoutingError err
  GotoRoute uri -> do
    io_ . pushURI $ toURI uri
    issue $ SetURI uri
  SetURI Index -> this .= Model Index False
  SetURI uri -> do
    open <-
      use this <&> \case
        RoutingError _ -> False
        Model _ open -> open
    this .= Model uri open
  SetPRDOpen open ->
    use this >>= \case
      RoutingError _ -> pure () -- TEMP FIXME
      Model uri _ -> this .= Model uri open

routerComponent :: (Model -> View Model Action) -> (Route -> ProductRequirementDocument) -> Model -> Component parent Model Action
routerComponent routerView routeToPRD uri =
  (component uri (updateModel routeToPRD) routerView)
    { subs =
        [ routerSub $ \case
            Left err -> ServerError . ms $ show err
            Right uri' -> SetURI uri'
        ]
    }
