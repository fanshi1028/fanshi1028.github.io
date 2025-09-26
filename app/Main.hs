{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
#ifndef PRODUCTION
{-# LANGUAGE TemplateHaskell #-}
#endif

module Main where

#ifndef PRODUCTION
import Data.FileEmbed
#endif
import Data.Bifunctor
import Miso
import Miso.Router as Router
import Route
import Route.View
import Prelude hiding (rem, unlines)

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
          Right uri' -> Model uri' False
      )
  )
#ifndef PRODUCTION
    {
      styles = [Style $ ms $(embedFileRelative "static/output.css")]
     , logLevel = DebugAll
    }
#endif
