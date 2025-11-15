{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Miso
import Miso.Router
import Route
import Route.View

-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------

main :: IO ()
main = run $ miso $ \route' ->
  ( routerComponent navView $ case route route' of
      Left err -> RoutingError err
      Right uri' -> Model uri'
  )
#ifndef PRODUCTION
    {
       scripts = [Src "https://cdn.tailwindcss.com"]
     , styles = [Href "static/input.css"]
     , logLevel = DebugAll
    }
#endif
