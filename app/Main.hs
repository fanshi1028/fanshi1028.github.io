{-# LANGUAGE CPP #-}

module Main where

import App
import Miso
#ifdef LOCALDEV
import App.View
import Miso.Router
#endif

-----------------------------------------------------------------------------
#if defined(wasm32_HOST_ARCH) && !defined(LOCALDEV)
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------

main :: IO ()
#ifdef LOCALDEV
main = reload . startApp defaultEvents . app $ toURI Index
#endif
#ifndef LOCALDEV
main = run $ miso defaultEvents app
#endif
