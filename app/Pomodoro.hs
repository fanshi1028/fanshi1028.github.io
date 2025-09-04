{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Pomodoro where

import qualified Clock
import Data.List.NonEmpty
import Data.Maybe
import Data.Time
import Miso
import Miso.Html
import Miso.Html.Property
import Miso.Lens

data Pomodoro
  = Pomodoro DiffTime
  | ShortBreak DiffTime
  | LongBreak DiffTime
  deriving (Eq)

data PomodoroQueue
  = PomodoroQueue
      { _pomodoroPastQueue :: [Pomodoro],
        _pomodoroRestQueue :: NonEmpty Pomodoro
      }
  | PomodoroQueueDone (NonEmpty Pomodoro)
  deriving (Eq)

data PomodoroSettings = PomodoroSettings
  { _pomodoro :: DiffTime,
    _shortBreak :: DiffTime,
    _longBreak :: DiffTime
  }
  deriving (Eq)

data Model = Model
  { _settings :: PomodoroSettings,
    _pomodoroQueue :: PomodoroQueue,
    _timeLeft :: DiffTime
  }
  deriving (Eq)

timeLeft :: Lens Model DiffTime
timeLeft = lens _timeLeft $ \record x -> record {_timeLeft = x}

settings :: Lens Model PomodoroSettings
settings = lens _settings $ \record x -> record {_settings = x}

pomodoroQueue :: Lens Model PomodoroQueue
pomodoroQueue = lens _pomodoroQueue $ \record x -> record {_pomodoroQueue = x}

defaultPomodoroQueue :: PomodoroQueue
defaultPomodoroQueue =
  PomodoroQueue [] $
    Pomodoro defaultPomodoro
      :| [ ShortBreak defaultShortBreak,
           Pomodoro defaultPomodoro,
           ShortBreak defaultShortBreak,
           Pomodoro defaultPomodoro,
           ShortBreak defaultShortBreak,
           Pomodoro defaultPomodoro,
           LongBreak defaultLongBreak
         ]

data Action
  = SetPomodoro
  | SetShortBreak
  | SetLongBreak
  | FastForwardToTheNextStage

updateModel :: Action -> Transition Model Action
updateModel = \case
  FastForwardToTheNextStage -> do
    use pomodoroQueue >>= \case
      PomodoroQueueDone _ -> pure ()
      PomodoroQueue past (current :| future) -> case future of
        [] -> pomodoroQueue .= PomodoroQueueDone (current :| past)
        current' : future' -> do
          pomodoroQueue .= PomodoroQueue (current : past) (current' :| future')
          timeLeft .= case current' of
            Pomodoro t -> t
            ShortBreak t -> t
            LongBreak t -> t
  _ -> error "TEMP FIXME: SetPomodoro | SetShortBreak | SetLongBreak not implemented."

viewModel :: Model -> View Model Action
viewModel m =
  div_
    [class_ "flex flex-col h-screen container mx-auto"]
    [ h1_ [class_ "sr-only"] [text "Pomodoro"],
      div_ [class_ "flex flex-row items-center justify-center gap-8 py-10 border rounded-lg my-auto"] $
        [ div_ [class_ "flex flex-col gap-6"] [settingsView, currentPomodoroView],
          pomodoroQueueView,
          button_ [onClick FastForwardToTheNextStage] ["TEMP forward testing button FIXME"]
        ]
    ]
  where
    pomodoroView mCls pomodoro =
      div_ [class_ $ "flex flex-row gap-4 " <> fromMaybe "" mCls] $
        let (prefix, time) = case pomodoro of
              Pomodoro t -> ("Pomodoro:", t)
              ShortBreak t -> ("Short Break:", t)
              LongBreak t -> ("Long Break:", t)
         in [p_ [] [text prefix], span_ [] [text . ms $ formatTime defaultTimeLocale "%M:%00ES" time]]

    settingsView =
      div_ [class_ "flex flex-col gap-6"] $
        [ h2_ [class_ "sr-only"] ["Settings"],
          div_ [class_ "flex flex-row gap-8"] $
            [ pomodoroView Nothing $ Pomodoro m._settings._pomodoro,
              div_
                [class_ "flex flex-row gap-4"]
                [ p_ [] [text "Short Break"],
                  p_ [] [text . ms $ formatTime defaultTimeLocale "%M:%00ES" m._settings._shortBreak]
                ],
              div_
                [class_ "flex flex-row gap-4"]
                [ p_ [] [text "Long Break:"],
                  p_ [] [text . ms $ formatTime defaultTimeLocale "%M:%00ES" m._settings._longBreak]
                ]
            ]
        ]

    currentPomodoroView =
      div_ [id_ "stopwatch", class_ "bg-neutral-600 p-4 rounded-lg"]
        +> ( ( component
                 (Clock.Model False (realToFrac m._timeLeft) Nothing)
                 Clock.updateModel
                 Clock.viewModel
             )
               { bindings = [timeLeft <--> Clock.timeLeft]
               }
           )

    pomodoroQueueView =
      div_ [] $
        [ h2_ [class_ "sr-only"] [text "Pomodoro Queue"],
          ul_
            [class_ "flex flex-col gap-3"]
            $ let q = m._pomodoroQueue
                  current :| rest = q._pomodoroRestQueue
                  currentView =
                    li_
                      [class_ "border-2 border-neutral-400 p-4 uppercase tracking-tight rounded text-center text-neutral-500 font-bold text-lg my-3"]
                      [ case current of
                          Pomodoro _ -> "Pomodoro"
                          ShortBreak _ -> "Short Break"
                          LongBreak _ -> "Long Break"
                          -- (Just "text-center text-neutral-500 font-bold text-lg")
                      ]
                  futureView = (li_ [class_ "px-2"] . (: []) . pomodoroView (Just "text-neutral-600")) <$> rest
               in foldl'
                    (\acc i -> li_ [class_ "px-2"] [pomodoroView (Just "text-neutral-400") i] : acc)
                    (currentView : futureView)
                    q._pomodoroPastQueue
        ]

defaultPomodoro, defaultShortBreak, defaultLongBreak :: DiffTime
defaultLongBreak = secondsToDiffTime $ 15 * 60
defaultShortBreak = secondsToDiffTime $ 5 * 60
defaultPomodoro = secondsToDiffTime $ 25 * 60
