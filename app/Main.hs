{-# LANGUAGE CPP #-}

module Main where

import App
import Miso

-----------------------------------------------------------------------------
#if defined(wasm32_HOST_ARCH) && defined(PRODUCTION)
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------

main :: IO ()
main = run $ miso defaultEvents app
