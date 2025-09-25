{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

#ifndef WASM
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
  (routerComponent routerView uri)
#ifndef WASM
    {
      styles = [Style $ ms $(embedFileRelative "static/output.css")]
     , logLevel = DebugAll
    }
#endif
