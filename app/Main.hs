{-# LANGUAGE CPP #-}

module Main where

import App
import Control.Concurrent
import Miso

-----------------------------------------------------------------------------
#if defined(wasm32_HOST_ARCH) && !defined(LOCALDEV)
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------

main :: IO ()
#ifdef LOCALDEV
main = () <$ forkIO (reload $ miso defaultEvents app)
#endif
#ifndef LOCALDEV
main = run $ miso defaultEvents app
#endif
