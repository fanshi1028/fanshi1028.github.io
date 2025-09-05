{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Clock
import Data.Aeson
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
          ( Pomodoro.PomodoroSettings
              (Pomodoro.ValueWithValidation (ms $ show Pomodoro.defaultPomodoro) $ pure Pomodoro.defaultPomodoro)
              (Pomodoro.ValueWithValidation (ms $ show Pomodoro.defaultShortBreak) $ pure Pomodoro.defaultShortBreak)
              (Pomodoro.ValueWithValidation (ms $ show Pomodoro.defaultLongBreak) $ pure Pomodoro.defaultLongBreak)
          )
          []
          Pomodoro.defaultCurrentPomodoro
          Pomodoro.defaultPomodoroFutureQueue
      )
      Pomodoro.updateModel
      Pomodoro.viewModel
  )
    { events = defaultEvents,
      styles = [Style $ ms $(embedFileRelative "static/output.css")],
      mailbox = \v -> case fromJSON v of
        Error _ -> Nothing
        Success Clock.ClockDoneMessage -> Just Pomodoro.Next
    }
