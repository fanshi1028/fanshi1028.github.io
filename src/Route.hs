{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Route (Route (..), Action (GotoRoute), routerComponent) where

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

type Model = Either MisoString Route

updateModel :: (Route -> ProductRequirementDocument) -> Action -> Effect parent Model Action
updateModel routeToPRD = \case
  ServerError err -> this .= Left err
  GotoRoute uri -> do
    io_ . pushURI $ toURI uri
    issue $ SetURI uri
  SetURI uri -> do
    this .= Right uri
    publish prdTopic $ routeToPRD uri

routerComponent :: (Model -> View Model Action) -> (Route -> ProductRequirementDocument) -> Model -> Component parent Model Action
routerComponent routerView routeToPRD uri =
  (component uri (updateModel routeToPRD) routerView)
    { subs =
        [ routerSub $ \case
            Left err -> ServerError . ms $ show err
            Right uri' -> SetURI uri'
        ]
    }
