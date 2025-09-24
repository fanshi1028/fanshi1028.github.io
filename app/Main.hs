{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

#ifndef WASM
import Data.FileEmbed
#endif
import GHC.Generics
import Home
import Miso
import Miso.Html
import Miso.Router as Router
import Pomodoro
import Prelude hiding (rem, unlines)

-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------

main :: IO ()
main = run $ miso $ \url ->
  let app = case url of
        URI "" "" _ -> home
        URI "pomodoro" "" _ -> pomodoroApp
        _ -> component () noop $ \() -> p_ [] ["TEMP FIXME 404"]
   in app
#ifndef WASM
       { styles = [Style $ ms $(embedFileRelative "static/output.css")] }
#endif

data Route = Index | Pomodoro
  deriving stock (Generic)
  deriving anyclass (Router)
