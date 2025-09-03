{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.FileEmbed (embedFileRelative)
import Miso
import qualified Pomodoro
import Prelude hiding (rem, unlines)

-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------

main :: IO ()
main = run $ startApp app

app :: App Pomodoro.Model Pomodoro.Action
app =
  ( component
      ( Pomodoro.Model
          (Pomodoro.PomodoroSettings Pomodoro.defaultPomodoro Pomodoro.defaultShortBreak Pomodoro.defaultLongBreak)
          Pomodoro.defaultPomodoroQueue
          Pomodoro.defaultPomodoro
      )
      Pomodoro.updateModel
      Pomodoro.viewModel
  )
    { events = defaultEvents,
      styles =
        [ Style $ ms $(embedFileRelative "static/output.css")
        ]
    }
