{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Pomodoro where

import qualified Clock
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

pomodoroToDiffTime :: Pomodoro -> DiffTime
pomodoroToDiffTime = \case
  Pomodoro t -> t
  ShortBreak t -> t
  LongBreak t -> t

data PomodoroSettings = PomodoroSettings
  { _pomodoro :: DiffTime,
    _shortBreak :: DiffTime,
    _longBreak :: DiffTime
  }
  deriving (Eq)

data Model = Model
  { _settings :: PomodoroSettings,
    _pomodoroPastQueue :: [Pomodoro],
    _currentPomodoro :: Pomodoro,
    _pomodoroFutureQueue :: [Pomodoro]
  }
  deriving (Eq)

settings :: Lens Model PomodoroSettings
settings = lens _settings $ \record x -> record {_settings = x}

pomodoroPastQueue :: Lens Model [Pomodoro]
pomodoroPastQueue = lens _pomodoroPastQueue $ \record x -> record {_pomodoroPastQueue = x}

currentPomodoro :: Lens Model Pomodoro
currentPomodoro = lens _currentPomodoro $ \record x -> record {_currentPomodoro = x}

pomodoroFutureQueue :: Lens Model [Pomodoro]
pomodoroFutureQueue = lens _pomodoroFutureQueue $ \record x -> record {_pomodoroFutureQueue = x}

defaultCurrentPomodoro :: Pomodoro
defaultCurrentPomodoro = Pomodoro defaultPomodoro

defaultPomodoroFutureQueue :: [Pomodoro]
defaultPomodoroFutureQueue =
  [ ShortBreak defaultShortBreak,
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
  | Next
  deriving (Eq)

updateModel :: Action -> Transition Model Action
updateModel = \case
  Next -> do
    current <- use currentPomodoro
    pomodoroPastQueue %= (current :)
    use pomodoroFutureQueue >>= \case
      [] -> pure ()
      current' : future -> do
        currentPomodoro .= current'
        pomodoroFutureQueue .= future
  _ -> error "TEMP FIXME: SetPomodoro | SetShortBreak | SetLongBreak not implemented."

viewModel :: Model -> View Model Action
viewModel m =
  div_
    [class_ "flex flex-col h-screen container mx-auto"]
    [ h1_ [class_ "sr-only"] [text "Pomodoro"],
      div_ [class_ "flex flex-row items-center justify-center gap-8 py-10 border rounded-lg my-auto"] $
        [div_ [class_ "flex flex-col gap-6"] [settingsView, currentPomodoroView], pomodoroQueueView]
    ]
  where
    pomodoroView mCls pomodoro =
      div_ [class_ $ "flex flex-row gap-4 " <> fromMaybe "" mCls] $
        [ p_
            []
            [ text $ case pomodoro of
                Pomodoro _ -> "Pomodoro:"
                ShortBreak _ -> "Short Break:"
                LongBreak _ -> "Long Break:"
            ],
          span_ [] [text . ms . formatTime defaultTimeLocale "%M:%00ES" $ pomodoroToDiffTime pomodoro]
        ]

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
                 (Clock.Model False (pomodoroToDiffTime m._currentPomodoro) Nothing)
                 Clock.updateModel
                 Clock.viewModel
             )
               { bindings = [ParentToChild (pomodoroToDiffTime . _currentPomodoro) (_set Clock.timeLeft)],
                 initialAction = Just Clock.Start
               }
           )

    pomodoroQueueView =
      div_ [] $
        [ h2_ [class_ "sr-only"] [text "Pomodoro Queue"],
          ul_ [class_ "flex flex-col gap-3"] $
            let pastItemView i = li_ [class_ "px-2"] [pomodoroView (Just "text-neutral-400") i]
                futureItemView i = li_ [class_ "px-2"] [pomodoroView (Just "text-neutral-600") i]
             in let currentView =
                      li_
                        [class_ "border-2 border-neutral-400 p-4 uppercase tracking-tight rounded text-center text-neutral-500 font-bold text-lg my-3"]
                        [ case m._currentPomodoro of
                            Pomodoro _ -> "Pomodoro"
                            ShortBreak _ -> "Short Break"
                            LongBreak _ -> "Long Break"
                        ]
                 in foldl' (\acc i -> pastItemView i : acc) (currentView : (futureItemView <$> m._pomodoroFutureQueue)) m._pomodoroPastQueue
        ]

defaultPomodoro, defaultShortBreak, defaultLongBreak :: DiffTime
defaultLongBreak = secondsToDiffTime $ 15 * 60
defaultShortBreak = secondsToDiffTime $ 5 * 60
defaultPomodoro = secondsToDiffTime $ 25 * 60
