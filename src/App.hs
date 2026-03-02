{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module App (app) where

import App.View
import Control.Monad
import Miso
import Miso.Lens
import Miso.Router
import View.ProductRequirementDocument

#ifdef LOCALDEV
import Embed.Dev
#endif

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  SetRoutingError err -> this .= RoutingError err
  GotoRoute uri -> do
    io_ . pushURI $ toURI uri
    issue $ SetURI uri
  SetURI uri -> this .= Model uri False
  SetPRDOpen setOpen -> io_ . void $ do
    prdDialgoue <- getElementById prdDialogueId
    prdDialgoue # (if setOpen then "showModal" else "close") $ ()
  AfterLoaded ->
    get >>= \case
      RoutingError err -> pure ()
      Model uri _ -> put $ Model uri False

app :: URI -> Component parent Model Action
app route' =
  (component model updateModel viewModel)
    { subs =
        [ routerSub $ \case
            Left err -> SetRoutingError err
            Right uri' -> SetURI uri'
        ],
      mount = Just AfterLoaded,
      scripts,
      styles,
      logLevel
    }
  where
    model = case route route' of
      Left err -> RoutingError err
      Right uri' -> Model uri' True
#ifdef PRODUCTION
    scripts = []
    styles = []
    logLevel = Off
#endif
-- NOTE: local-dev? production?
#ifdef LOCALDEV
    scripts = [Src "https://cdn.tailwindcss.com" False]
    styles = [Style $ ms staticInputCSS]
    logLevel = DebugAll
#endif
