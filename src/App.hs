{-# LANGUAGE CPP #-}

module App (app) where

import App.Update
import App.View
import Miso
import Miso.Router

app :: URI -> Component parent Model Action
app route' =
  (component model updateModel viewModel)
    { subs =
        [ routerSub $ \case
            Left err -> SetRoutingError err
            Right uri' -> SetURI uri'
        ],
      initialAction = Just AfterLoaded,
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
#ifndef PRODUCTION
    scripts = [Src $ ms "https://cdn.tailwindcss.com"]
    styles = [Href $ ms "static/input.css"]
    logLevel = DebugAll
#endif
