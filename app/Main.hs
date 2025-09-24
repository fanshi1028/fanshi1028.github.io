{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

#ifndef WASM
import Data.FileEmbed
#endif
import Data.Bifunctor
import Home
import Miso
import Miso.Html
import Miso.Router as Router
import Pomodoro
import Route
import Prelude hiding (rem, unlines)

-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------

main :: IO ()
main = run $ miso $ \uri ->
  (component (first (ms . show) $ route @Route uri) updateModel viewModel)
    { subs =
        [ routerSub $ \case
            Left err -> ServerError . ms $ show err
            Right uri' -> SetURI uri'
        ]
#ifndef WASM
     , styles = [Style $ ms $(embedFileRelative "static/output.css")]
     , logLevel = DebugAll
#endif
    }

viewModel :: Model -> View Model Action
viewModel = \case
  Right Index -> home
  Right Pomodoro -> div_ [key_ @MisoString "pomodoro-app"] +> pomodoroApp
  Left err' -> view500 err'

