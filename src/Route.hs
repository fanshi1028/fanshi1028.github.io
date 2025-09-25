{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Route (Route (..), Action (GotoRoute), routerComponent) where

import Data.Aeson hiding ((.=))
import GHC.Generics
import Miso
import Miso.Lens
import Miso.Router

data Route = Index | Pomodoro
  deriving stock (Eq, Show, Enum, Bounded, Generic)
  deriving anyclass (Router)

data Action
  = ServerError MisoString
  | GotoRoute Route
  | SetURI Route

type Model = Either MisoString Route

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  ServerError err -> this .= Left err
  GotoRoute uri -> do
    io_ . pushURI $ toURI uri
    issue $ SetURI uri
  SetURI uri -> this .= Right uri

routerComponent :: (Model -> View Model Action) -> Model -> Component parent Model Action
routerComponent routeToView uri =
  (component uri updateModel routeToView)
    { subs =
        [ routerSub $ \case
            Left err -> ServerError . ms $ show err
            Right uri' -> SetURI uri'
        ]
    }
