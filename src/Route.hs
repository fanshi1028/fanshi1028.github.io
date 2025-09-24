{-# LANGUAGE OverloadedStrings #-}

module Route where

import Home
import Miso
import Miso.Html.Element
import Miso.Lens
import Miso.Router
import Pomodoro
import Route.Types

-- routingTopic :: Topic Route
-- routingTopic = topic "routing"

-- | NotFound URI
updateModel :: Action -> Effect parent Model Action
updateModel = \case
  -- subscribe routingTopic PushURI ServerError
  -- NotFound uri -> this .= Left (prettyURI uri)
  ServerError err -> this .= Left err
  PushURI uri -> do
    io_ . pushURI $ toURI uri
    issue $ SetURI uri
  SetURI uri -> this .= Right uri

-- view404 :: URI -> View model action
-- view404 uri =
--   div_
--     []
--     [ text "TEMP FIXME: Not found 404",
--       br_ [],
--       text $ prettyURI uri
--     ]

view500 :: MisoString -> View model action
view500 err =
  div_
    []
    [ text "TEMP FIXME: Internal Server Error 500",
      br_ [],
      text err
    ]

routeToView :: Model -> View Model Action
routeToView = \case
  Right Index -> home
  Right Pomodoro -> div_ [key_ @MisoString "pomodoro-app"] +> pomodoroApp
  Left err' -> view500 err'

routerComponent :: Model -> Component parent Model Action
routerComponent uri =
  (component uri updateModel routeToView)
    { subs =
        [ routerSub $ \case
            Left err -> ServerError . ms $ show err
            Right uri' -> SetURI uri'
        ]
    }
