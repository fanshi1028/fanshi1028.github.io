{-# LANGUAGE CPP #-}

module Main where

import App
import Miso

-----------------------------------------------------------------------------
#ifdef wasm32_HOST_ARCH
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------

main :: IO ()
main = run $ miso defaultEvents app
