{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Route where

import Data.Aeson hiding ((.=))
import GHC.Generics
import Miso
import Miso.Html.Element
import Miso.Lens
import Miso.Router

data Route = Index | Pomodoro
  deriving stock (Eq, Generic)
  deriving anyclass (Router, ToJSON, FromJSON)

-- routingTopic :: Topic Route
-- routingTopic = topic "routing"

data Action
  = ServerError MisoString
  | PushURI Route
  | SetURI Route

type Model = Either MisoString Route

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
