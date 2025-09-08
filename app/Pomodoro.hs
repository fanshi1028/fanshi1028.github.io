{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Pomodoro where

import qualified Clock
import Control.Applicative
import Control.Category
import Data.Aeson as Aeson hiding ((.=))
import Data.List.NonEmpty
import Data.Maybe
import Data.Time
import GHC.Natural
import Miso
import Miso.Html
import Miso.Html.Property
import Miso.Lens
import Network.URI.Static
import ProductRequirementDocument
import Text.Read
import Validation as Validation hiding (validation)
import Prelude hiding ((.))

data PomodoroStage = Pomodoro | ShortBreak | LongBreak deriving (Eq, Enum, Bounded)

stageToMisoString :: PomodoroStage -> MisoString
stageToMisoString = \case
  Pomodoro -> "Pomodoro"
  ShortBreak -> "Short Break"
  LongBreak -> "Long Break"

data Pomodoro = MkPomodoro
  { _pomodoroStage :: PomodoroStage,
    _pomodoroTime :: DiffTime
  }
  deriving (Eq)

newtype PomodoroMinuteSettingValidationError = PomodoroMinuteSettingValidationError {unPomodoroMinuteSettingValidationError :: MisoString} deriving (Eq)

data ValueWithValidation = ValueWithValidation
  { _value :: MisoString,
    _validation :: Validation (NonEmpty PomodoroMinuteSettingValidationError) Natural
  }
  deriving (Eq)

value :: Lens ValueWithValidation MisoString
value = lens _value $ \record x -> record {_value = x}

validation :: Lens ValueWithValidation (Validation (NonEmpty PomodoroMinuteSettingValidationError) Natural)
validation = lens _validation $ \record x -> record {_validation = x}

data PomodoroSettings = PomodoroSettings
  { _pomodoro :: ValueWithValidation,
    _shortBreak :: ValueWithValidation,
    _longBreak :: ValueWithValidation
  }
  deriving (Eq)

pomodoro, shortBreak, longBreak :: Lens PomodoroSettings ValueWithValidation
pomodoro = lens _pomodoro $ \record x -> record {_pomodoro = x}
shortBreak = lens _shortBreak $ \record x -> record {_shortBreak = x}
longBreak = lens _longBreak $ \record x -> record {_longBreak = x}

stageToSettingLens :: PomodoroStage -> Lens PomodoroSettings ValueWithValidation
stageToSettingLens = \case
  Pomodoro -> pomodoro
  ShortBreak -> shortBreak
  LongBreak -> longBreak

data Model = Model
  { _settings :: PomodoroSettings,
    _settingsOpen :: Bool,
    _pomodoroPastQueue :: [Pomodoro],
    _currentPomodoro :: Pomodoro,
    _pomodoroFutureQueue :: [Pomodoro]
  }
  deriving (Eq)

settings :: Lens Model PomodoroSettings
settings = lens _settings $ \record x -> record {_settings = x}

settingsOpen :: Lens Model Bool
settingsOpen = lens _settingsOpen $ \record x -> record {_settingsOpen = x}

pomodoroPastQueue :: Lens Model [Pomodoro]
pomodoroPastQueue = lens _pomodoroPastQueue $ \record x -> record {_pomodoroPastQueue = x}

currentPomodoro :: Lens Model Pomodoro
currentPomodoro = lens _currentPomodoro $ \record x -> record {_currentPomodoro = x}

pomodoroFutureQueue :: Lens Model [Pomodoro]
pomodoroFutureQueue = lens _pomodoroFutureQueue $ \record x -> record {_pomodoroFutureQueue = x}

naturalAsMinutesToDiffTime :: Natural -> DiffTime
naturalAsMinutesToDiffTime n = secondsToDiffTime $ 60 * fromIntegral n

defaultCurrentPomodoro :: Pomodoro
defaultCurrentPomodoro = MkPomodoro Pomodoro $ naturalAsMinutesToDiffTime defaultPomodoro

defaultPomodoroFutureQueue :: [Pomodoro]
defaultPomodoroFutureQueue =
  [ MkPomodoro ShortBreak $ naturalAsMinutesToDiffTime defaultShortBreak,
    MkPomodoro Pomodoro $ naturalAsMinutesToDiffTime defaultPomodoro,
    MkPomodoro ShortBreak $ naturalAsMinutesToDiffTime defaultShortBreak,
    MkPomodoro Pomodoro $ naturalAsMinutesToDiffTime defaultPomodoro,
    MkPomodoro ShortBreak $ naturalAsMinutesToDiffTime defaultShortBreak,
    MkPomodoro Pomodoro $ naturalAsMinutesToDiffTime defaultPomodoro,
    MkPomodoro LongBreak $ naturalAsMinutesToDiffTime defaultLongBreak
  ]

data Action = ToggleSettingsOpen | Set PomodoroStage MisoString | Next deriving (Eq)

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  ToggleSettingsOpen -> settingsOpen %= not
  Next -> do
    current <- use currentPomodoro
    pomodoroPastQueue %= (current :)
    use pomodoroFutureQueue >>= \case
      [] -> pure ()
      current' : future -> do
        currentPomodoro .= current'
        pomodoroFutureQueue .= future
  Set (stageToSettingLens -> stageLens) str -> do
    let validateMax120 n = failureIf (n > 120) (PomodoroMinuteSettingValidationError "input must <= 120")
        validateMin5 n = failureIf (n < 5) (PomodoroMinuteSettingValidationError "input must >= 5")
        validateMultiple5 n = failureIf (n `rem` 5 /= 0) (PomodoroMinuteSettingValidationError "input must be a multiple of 5")
    value . stageLens . settings .= str
    validation . stageLens . settings .= case readEither $ fromMisoString str of
      Left err -> failure (PomodoroMinuteSettingValidationError $ "input must be in number format: " <> ms err)
      Right n -> validateAll [validateMultiple5, validateMin5, validateMax120] n

viewModel :: Model -> View Model Action
viewModel m =
  div_
    [class_ "flex flex-col h-screen container mx-auto"]
    [ h1_ [class_ "sr-only"] [text "Pomodoro"],
      div_ [class_ "flex flex-row items-center justify-center gap-8 py-10 border rounded-lg my-auto bg-neutral-200"] $
        [div_ [class_ "flex flex-col gap-6"] [settingsView, currentPomodoroView], pomodoroQueueView]
    ]
  where
    pomodoroView mCls (MkPomodoro stage time) =
      div_ [class_ $ "flex flex-row gap-4 " <> fromMaybe "" mCls] $
        [ p_
            []
            [text $ stageToMisoString stage <> ": "],
          span_ [] [text . ms $ formatTime defaultTimeLocale "%M:%00ES" time]
        ]

    settingView stage =
      let v = m ^. (stageToSettingLens stage) . settings
       in div_
            [class_ "flex flex-row gap-4"]
            [ p_ [] [text $ stageToMisoString stage],
              div_
                [class_ "flex flex-col"]
                [ input_
                    [ class_ "bg-transparent",
                      type_ "number",
                      max_ "90",
                      min_ "5",
                      step_ "5",
                      value_ v._value,
                      onChange $ Set stage
                    ],
                  -- p_ [] [text . ms $ formatTime defaultTimeLocale "%M:%00ES" m._settings._shortBreak]
                  case v._validation of
                    Validation.Success _ -> div_ [] []
                    Failure (toList -> errs) ->
                      div_ [class_ "flex flex-col"] $
                        p_ [] . (: []) . text . unPomodoroMinuteSettingValidationError <$> errs
                ]
            ]

    settingsView =
      div_ [] $
        [ h2_ [class_ "sr-only"] ["Settings"],
          if m ^. settingsOpen
            then
              div_
                [class_ "flex flex-col gap-4"]
                [ div_ [class_ "flex flex-row gap-8"] $ settingView <$> [minBound .. maxBound],
                  div_ [class_ "flex flex-row gap-2 justify-around"] $
                    [ button_ [] ["Apply"],
                      button_ [onClick ToggleSettingsOpen] ["Close"]
                    ]
                ]
            else button_ [onClick ToggleSettingsOpen] ["Open Settings"]
        ]

    currentPomodoroView =
      div_ [id_ "stopwatch", class_ "bg-neutral-600 p-4 rounded-lg shadow-lg shadow-neutral-600"]
        +> ( ( component
                 (Clock.Model False (m._currentPomodoro._pomodoroTime) Nothing)
                 Clock.updateModel
                 Clock.viewModel
             )
               { bindings = [ParentToChild (_pomodoroTime . _currentPomodoro) (_set Clock.timeLeft)],
                 initialAction = Just Clock.Start
               }
           )

    pomodoroQueueView =
      div_ [] $
        [ h2_ [class_ "sr-only"] [text "Pomodoro Queue"],
          ul_ [class_ "flex flex-col gap-3"] $
            let pastItemView i = li_ [class_ "px-2"] [pomodoroView (Just "text-neutral-400 font-semibold") i]
                futureItemView i = li_ [class_ "px-2"] [pomodoroView (Just "text-neutral-500 font-semibold") i]
             in let currentView =
                      li_
                        [class_ "border-2 border-neutral-500/80 p-6 uppercase tracking-tight rounded text-center text-neutral-500 font-bold text-xl my-3"]
                        [text $ stageToMisoString m._currentPomodoro._pomodoroStage]
                 in foldl' (\acc i -> pastItemView i : acc) (currentView : (futureItemView <$> m._pomodoroFutureQueue)) m._pomodoroPastQueue
        ]

defaultPomodoro, defaultShortBreak, defaultLongBreak :: Natural
defaultLongBreak = 15
defaultShortBreak = 5
defaultPomodoro = 25

pomodoroComponent :: Component parent Model Action
pomodoroComponent =
  ( component
      ( Pomodoro.Model
          ( Pomodoro.PomodoroSettings
              (Pomodoro.ValueWithValidation (ms $ show Pomodoro.defaultPomodoro) $ pure Pomodoro.defaultPomodoro)
              (Pomodoro.ValueWithValidation (ms $ show Pomodoro.defaultShortBreak) $ pure Pomodoro.defaultShortBreak)
              (Pomodoro.ValueWithValidation (ms $ show Pomodoro.defaultLongBreak) $ pure Pomodoro.defaultLongBreak)
          )
          False
          []
          Pomodoro.defaultCurrentPomodoro
          Pomodoro.defaultPomodoroFutureQueue
      )
      Pomodoro.updateModel
      Pomodoro.viewModel
  )
    { mailbox = \v -> case fromJSON v of
        Error _ -> Nothing
        Aeson.Success Clock.ClockDoneMessage -> Just Pomodoro.Next
    }

pomodoroPRD :: ProductRequirementDocument
pomodoroPRD =
  ProductRequirementDocument
    ( ProblemAlignment
        ( Problem
            "To build a simple Pomodoro app"
            "I need some quick and simple on my site as a MVP to showcase my programming skills"
            "So that my site is not empty and it will be a tool that I could use"
            "Bonus It is a tool that people could use and potentially bring traffic"
            ""
            ""
            :| []
        )
        "Starting from feature, evolving into a nice UI, don't overengineering it."
        ("A functional pomodoro timer" :| ["UI/UX should be simple and modern"])
    )
    ( SolutionAlignment
        "Settings -> Timer"
        [ "Users can set their desired time for pomodoro and breaks",
          "Users can start and stop the timer",
          "Users can fastforward to skip to the next stage"
        ]
        [ OpenIssues "Tech Stack" ["Use Miso and Tailwind, as the whole site is powered by it"],
          OpenIssues
            "Timer Settings Reasonable Values"
            [ "5 mins as the unit, I don't see we need more flexibility here, make it simple",
              "More than 5 mins, It is too fragment to do reasonable work or have a reasonable rest if the time is too short",
              "Less than 120 mins, I don't think and human could sustain a undistrubed focus for that long while being poductive",
              "Following the tried and true 4 short break then a long break, I don't see the value of extra flexibilify as we offer to tweak the time for each stage already"
            ]
        ]
        [Reference [uri|https://pomofocus.io/|] ["I don't need those complex features like it does but I like the simple and clean ui"]]
    )
    []
