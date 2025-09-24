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
import Miso
import Miso.Html
import Miso.Router as Router
import Prelude hiding (rem, unlines)
import Home

-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------

main :: IO ()
main = run $ miso $ \case
  URI "" "" _ -> home {
      events = defaultEvents
#ifndef WASM
    , styles = [Style $ ms $(embedFileRelative "static/output.css")]
#endif
    }
  _ -> component () noop $ \() -> p_ [] ["TEMP FIXME 404"]

data Route = Index | Pomodoro
  deriving stock (Generic)
  deriving anyclass (Router)
