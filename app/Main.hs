{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.FileEmbed (embedFileRelative)
import Miso
import Miso.Html.Element
import Miso.Html.Property
import qualified Pomodoro
import Prelude hiding (rem, unlines)

-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------

main :: IO ()
main = run $ startApp app

app :: App () action
app = component () noop $ \() ->
  div_
    []
    [ div_ [id_ "pomodoro"]
        +> Pomodoro.pomodoroComponent
          { events = defaultEvents,
            styles = [Style $ ms $(embedFileRelative "static/output.css")]
          }
    ]
