{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Bifunctor
import Miso
import Miso.Router as Router
import Route
import Route.View

-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------

main :: IO ()
main = run $ miso $ \(first (ms . show) . route @Route -> uri) ->
  ( routerComponent
      navView
      ( case uri of
          Left err -> RoutingError err
          Right uri' -> Model uri'
      )
  )
#ifndef PRODUCTION
    {
       scripts = [Src "https://cdn.tailwindcss.com"]
     , styles = [Href "static/input.css"]
     , logLevel = DebugAll
    }
#endif
