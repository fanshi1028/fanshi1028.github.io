{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Aeson as Aeson hiding ((.=))
import Data.Bifunctor
import Data.List.NonEmpty
import Data.Map as Map hiding (foldl', toList)
import Data.Maybe
import Data.Time
import GHC.Natural
import Miso hiding (Transition)
import Miso.CSS hiding (ms, rem)
import Miso.Html as HTML
import Miso.Html.Property as P
import Miso.Lens
import Miso.String hiding (foldl')
import Miso.Svg.Element
import Miso.Svg.Property hiding (max_, min_, path_)
import Network.URI.Static
import ProductRequirementDocument hiding (Action, Model, NoOp)
import Text.Read
import Validation as Validation hiding (validation)
import Prelude hiding ((.))

data PomodoroStage = Pomodoro | ShortBreak | LongBreak deriving (Eq, Show, Ord, Enum, Bounded)

stageToMisoString :: PomodoroStage -> MisoString
stageToMisoString = \case
  Pomodoro -> "Pomodoro"
  ShortBreak -> "Short Break"
  LongBreak -> "Long Break"

data Pomodoro = MkPomodoro
  { _pomodoroStage :: PomodoroStage,
    _pomodoroTime :: DiffTime
  }
  deriving stock (Eq, Show, Ord)

newtype PomodoroMinuteSettingValidationError = PomodoroMinuteSettingValidationError {unPomodoroMinuteSettingValidationError :: MisoString}
  deriving stock (Eq)
  deriving newtype (ToMisoString)

data ValueWithValidation = ValueWithValidation
  { _value :: MisoString,
    _validation :: Validation (NonEmpty PomodoroMinuteSettingValidationError) Natural
  }
  deriving stock (Eq)

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

newtype PomodoroQueueIndex = PomodoroQueueIndex Int deriving stock (Eq, Show, Ord)

data Transition = PastItemTransition PomodoroQueueIndex | FutureItemTransition PomodoroQueueIndex deriving stock (Eq, Show, Ord)

data Model = Model
  { _settings :: PomodoroSettings,
    _settingsOpen :: Bool,
    _pomodoroPastQueue :: [(Pomodoro, PomodoroQueueIndex)],
    _currentPomodoro :: (Pomodoro, PomodoroQueueIndex),
    _pomodoroFutureQueue :: [(Pomodoro, PomodoroQueueIndex)],
    _transitionMap :: Map Transition Bool
  }
  deriving (Eq)

transitionMap :: Lens Model (Map Transition Bool)
transitionMap = lens _transitionMap $ \record x -> record {_transitionMap = x}

settings :: Lens Model PomodoroSettings
settings = lens _settings $ \record x -> record {_settings = x}

settingsOpen :: Lens Model Bool
settingsOpen = lens _settingsOpen $ \record x -> record {_settingsOpen = x}

pomodoroPastQueue :: Lens Model [(Pomodoro, PomodoroQueueIndex)]
pomodoroPastQueue = lens _pomodoroPastQueue $ \record x -> record {_pomodoroPastQueue = x}

currentPomodoro :: Lens Model (Pomodoro, PomodoroQueueIndex)
currentPomodoro = lens _currentPomodoro $ \record x -> record {_currentPomodoro = x}

pomodoroFutureQueue :: Lens Model [(Pomodoro, PomodoroQueueIndex)]
pomodoroFutureQueue = lens _pomodoroFutureQueue $ \record x -> record {_pomodoroFutureQueue = x}

naturalAsMinutesToDiffTime :: Natural -> DiffTime
naturalAsMinutesToDiffTime n = secondsToDiffTime $ 60 * fromIntegral n

defaultCurrentPomodoro :: Pomodoro
defaultCurrentPomodoro = MkPomodoro Pomodoro $ naturalAsMinutesToDiffTime defaultPomodoro

defaultPomodoroFutureQueue :: [(Pomodoro, PomodoroQueueIndex)]
defaultPomodoroFutureQueue =
  Prelude.zip
    [ MkPomodoro ShortBreak $ naturalAsMinutesToDiffTime defaultShortBreak,
      MkPomodoro Pomodoro $ naturalAsMinutesToDiffTime defaultPomodoro,
      MkPomodoro ShortBreak $ naturalAsMinutesToDiffTime defaultShortBreak,
      MkPomodoro Pomodoro $ naturalAsMinutesToDiffTime defaultPomodoro,
      MkPomodoro ShortBreak $ naturalAsMinutesToDiffTime defaultShortBreak,
      MkPomodoro Pomodoro $ naturalAsMinutesToDiffTime defaultPomodoro,
      MkPomodoro LongBreak $ naturalAsMinutesToDiffTime defaultLongBreak
    ]
    $ PomodoroQueueIndex <$> [1 ..]

data Action
  = SwitchToPRD
  | ToggleSettingsOpen
  | Set PomodoroStage MisoString
  | ApplyPomodoroSettings
  | PreNextTransition
  | Next
  | PostNextTransition
  deriving stock (Eq, Show)

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  ApplyPomodoroSettings -> do
    pomodoro' <- use $ pomodoro . settings
    shortBreak' <- use $ shortBreak . settings
    longBreak' <- use $ longBreak . settings
    case (,,) <$> pomodoro'._validation <*> shortBreak'._validation <*> longBreak'._validation of
      Failure (err :| errs) -> do
        io_ . consoleWarn $ foldl' (\acc err' -> acc <> ", " <> ms err') (ms err) errs -- NOTE: ApplyPomodoroSettings button should not be active when validation failed
      Validation.Success
        ( naturalAsMinutesToDiffTime -> pomodoro'',
          naturalAsMinutesToDiffTime -> shortBreak'',
          naturalAsMinutesToDiffTime -> longBreak''
          ) -> do
          pomodoroFutureQueue
            %= fmap
              ( first $ \(MkPomodoro stage _) -> case stage of
                  LongBreak -> MkPomodoro stage longBreak''
                  ShortBreak -> MkPomodoro stage shortBreak''
                  Pomodoro -> MkPomodoro stage pomodoro''
              )
          settingsOpen .= False
  SwitchToPRD -> publish prdTopic pomodoroPRD
  ToggleSettingsOpen -> settingsOpen %= not
  PreNextTransition -> do
    startSub (show PreNextTransition) $ \sink -> do
      liftIO $ threadDelay 300000
      sink Next
    use pomodoroFutureQueue >>= \case
      [] -> pure () -- TEMP FIXME
      (_, idx) : _ -> transitionMap %= Map.insert (FutureItemTransition idx) False
  Next -> do
    startSub (show Next) $ \sink -> do
      liftIO $ threadDelay 300000
      sink PostNextTransition
    current@(_, idx) <- use currentPomodoro
    transitionMap %= Map.insert (PastItemTransition idx) False
    use pomodoroFutureQueue >>= \case
      [] -> pure () -- TEMP FIXME
      current'@(_, idx') : future -> do
        currentPomodoro .= current'
        pomodoroFutureQueue .= future
        transitionMap %= Map.delete (FutureItemTransition idx')

    use pomodoroFutureQueue >>= \case
      [] -> pure () -- TEMP FIXME
      (_, idx'') : _ -> do transitionMap %= Map.delete (PastItemTransition idx'')

    pomodoroPastQueue %= (current :)
  PostNextTransition ->
    use pomodoroPastQueue >>= \case
      [] -> pure () -- TEMP: should be impossible
      (_, idx) : _ -> transitionMap %= Map.insert (PastItemTransition idx) True
  Set (stageToSettingLens -> stageLens) str -> do
    let validateMax45 n = failureIf (n > 45) (PomodoroMinuteSettingValidationError "must <= 45")
        validateMin5 n = failureIf (n < 5) (PomodoroMinuteSettingValidationError "must >= 5")
        validateMultiple5 n = failureIf (n `rem` 5 /= 0) (PomodoroMinuteSettingValidationError "must be a multiple of 5")
    value . stageLens . settings .= str
    validation . stageLens . settings .= case readEither $ fromMisoString str of
      Left _ -> failure (PomodoroMinuteSettingValidationError $ "must be a number")
      Right n -> validateAll [validateMultiple5, validateMin5, validateMax45] n

viewModel :: Model -> View Model Action
viewModel m =
  div_
    [class_ "flex flex-col container mx-auto justify-center pb-6 min-h-screen bg-neutral-200"]
    [ h1_ [class_ "sr-only"] [text "Pomodoro"],
      button_ [onClick SwitchToPRD, class_ "sticky top-2 self-end mr-2"] [prdSwitchSVG "stroke-neutral-600 size-6"],
      pomodoroQueueView $ div_ [class_ $ "block relative my-6" <> sizeCls] [settingsView, currentPomodoroView]
    ]
  where
    sizeCls = " w-80 sm:w-96 h-64 sm:h-80"
    pomodoroView mCls (MkPomodoro stage time) =
      div_ [class_ $ "flex flex-row gap-4 " <> fromMaybe "" mCls] $
        [ p_
            []
            [text $ stageToMisoString stage <> ": "],
          span_ [] [text . ms $ formatTime defaultTimeLocale "%M:%00ES" time]
        ]

    settingView stage =
      let v = m ^. (stageToSettingLens stage) . settings
          disableIfOtherInvalidated = case traverse (\stage' -> (m ^. (stageToSettingLens stage') . settings)._validation) [minBound .. maxBound] of
            Validation.Success _ -> Prelude.id
            Failure _ -> case v._validation of
              Validation.Success _ -> (disabled_ :)
              Failure _ -> Prelude.id
          stageNameInputId = stageToMisoString stage <> " mins"
       in div_
            [class_ "w-4/5 has-[:invalid]:w-full flex flex-col-reverse has-[:invalid]:gap-2 has-[:invalid]:grid has-[:invalid]:grid-rows-2 has-[:invalid]:grid-cols-12 has-[:invalid]:col-span-2 has-[:invalid]:row-start-1 has-[:invalid]:mt-8"]
            [ input_
                ( disableIfOtherInvalidated
                    [ class_ "peer bg-neutral-200 text-neutral-600 text-lg font-bold rounded focus:ring-0 focus:border-0 focus:outline-1 focus:outline-neutral-800 w-full shadow-inner shadow-neutral-800 invalid:col-span-7 invalid:col-start-3 invalid:text-xl disabled:text-neutral-400 disabled:bg-neutral-300",
                      type_ "number",
                      required_ True,
                      max_ "45",
                      min_ "5",
                      step_ "5",
                      value_ v._value,
                      onInput $ Set stage,
                      P.id_ stageNameInputId
                    ]
                ),
              HTML.label_ [class_ "text-neutral-400 font-semibold peer-invalid:[writing-mode:sideways-lr] peer-invalid:col-span-2 peer-invalid:col-start-1 peer-invalid:row-start-1 peer-invalid:row-span-2 peer-invalid:text-lg peer-invalid:tracking-tight peer-invalid:text-right peer-invalid:ml-2", for_ stageNameInputId] [text $ stageToMisoString stage],
              case v._validation of
                Validation.Success _ -> div_ [] []
                Failure (toList -> errs) ->
                  ul_ [class_ "list-disc list-inside invisible peer-invalid:visible peer-invalid:col-span-9 peer-invalid:row-start-2 peer-invalid:col-start-3"] $
                    li_ [class_ "text-neutral-200 font-semibold leading-tight"] . (: []) . text . unPomodoroMinuteSettingValidationError <$> errs
            ]

    settingsView =
      div_
        [ class_ $ "absolute transition-transform duration-500" <> sizeCls,
          styleInline_ $ "backface-visibility:hidden;" <> if m._settingsOpen then "" else "transform:rotateY(180deg);"
        ]
        $ [ h2_ [class_ "sr-only"] ["Settings"],
            div_
              [ class_
                  "group grid grid-rows-2 grid-cols-2 place-items-center sm:flex sm:flex-row w-full h-full bg-neutral-600 justify-center items-center p-4 rounded-lg shadow-lg shadow-neutral-600"
              ]
              [ div_ [class_ "contents sm:flex sm:flex-col sm:gap-4 sm:grow"] $
                  settingView <$> [minBound .. maxBound],
                div_ [class_ "flex flex-row sm:flex-col gap-2 justify-around"] $
                  [ button_
                      [onClick ApplyPomodoroSettings, class_ "group-[:has(:invalid)]:hidden"]
                      [ p_ [class_ "sr-only"] ["Apply"],
                        svg_
                          [class_ "fill-none stroke-2 stroke-neutral-400 size-12", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 24 24"]
                          [path_ [strokeLinecap_ "round", strokeLinejoin_ "round", d_ "m4.5 12.75 6 6 9-13.5"]]
                      ],
                    button_
                      [onClick ToggleSettingsOpen, class_ "group-[:has(:invalid)]:absolute top-2 right-2"]
                      [ p_ [class_ "sr-only"] ["Close"],
                        svg_
                          [class_ "fill-none stroke-2 stroke-neutral-400 size-12", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 24 24"]
                          [path_ [strokeLinecap_ "round", strokeLinejoin_ "round", d_ "M6 18 18 6M6 6l12 12"]]
                      ]
                  ]
              ]
          ]

    currentPomodoroView =
      div_
        [ class_ $ "absolute transition-transform duration-500" <> sizeCls,
          styleInline_ $ "backface-visibility:hidden;" <> if m._settingsOpen then "transform:rotateY(180deg);" else ""
        ]
        [ button_
            [onClick ToggleSettingsOpen, class_ "absolute top-4 right-4"]
            [ p_ [class_ "sr-only"] ["Open Settings"],
              svg_
                [class_ "fill-none stroke-2 stroke-neutral-400 size-8 sm:size-10", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 24 24"]
                [ path_ [strokeLinecap_ "round", strokeLinejoin_ "round", d_ "M9.594 3.94c.09-.542.56-.94 1.11-.94h2.593c.55 0 1.02.398 1.11.94l.213 1.281c.063.374.313.686.645.87.074.04.147.083.22.127.325.196.72.257 1.075.124l1.217-.456a1.125 1.125 0 0 1 1.37.49l1.296 2.247a1.125 1.125 0 0 1-.26 1.431l-1.003.827c-.293.241-.438.613-.43.992a7.723 7.723 0 0 1 0 .255c-.008.378.137.75.43.991l1.004.827c.424.35.534.955.26 1.43l-1.298 2.247a1.125 1.125 0 0 1-1.369.491l-1.217-.456c-.355-.133-.75-.072-1.076.124a6.47 6.47 0 0 1-.22.128c-.331.183-.581.495-.644.869l-.213 1.281c-.09.543-.56.94-1.11.94h-2.594c-.55 0-1.019-.398-1.11-.94l-.213-1.281c-.062-.374-.312-.686-.644-.87a6.52 6.52 0 0 1-.22-.127c-.325-.196-.72-.257-1.076-.124l-1.217.456a1.125 1.125 0 0 1-1.369-.49l-1.297-2.247a1.125 1.125 0 0 1 .26-1.431l1.004-.827c.292-.24.437-.613.43-.991a6.932 6.932 0 0 1 0-.255c.007-.38-.138-.751-.43-.992l-1.004-.827a1.125 1.125 0 0 1-.26-1.43l1.297-2.247a1.125 1.125 0 0 1 1.37-.491l1.216.456c.356.133.751.072 1.076-.124.072-.044.146-.086.22-.128.332-.183.582-.495.644-.869l.214-1.28Z"],
                  path_ [strokeLinecap_ "round", strokeLinejoin_ "round", d_ "M15 12a3 3 0 1 1-6 0 3 3 0 0 1 6 0Z"]
                ]
            ],
          div_
            [ key_ @MisoString $ "stopwatch " <> ms (show (fst m._currentPomodoro)._pomodoroTime),
              class_ "bg-neutral-600 rounded-lg shadow-lg shadow-neutral-600 h-full w-full pt-6"
            ]
            +> ( ( component
                     (Clock.Model False ((fst m._currentPomodoro)._pomodoroTime) Nothing)
                     Clock.updateModel
                     Clock.viewModel
                 )
                   { -- bindings = [ParentToChild (_pomodoroTime . _currentPomodoro) (_set Clock.timeLeft)],
                     initialAction = Just Clock.Start
                   }
               )
        ]
    pomodoroQueueView currentView =
      div_
        [class_ "contents"]
        [ h2_ [class_ "sr-only"] [text "Pomodoro Queue"],
          ul_ [class_ "flex flex-col md:flex-row md:justify-around items-center gap-3"] $
            let pastItemView (i, idx) =
                  li_
                    [ class_ $
                        "px-2 "
                          <> ( case Map.lookup (PastItemTransition idx) m._transitionMap of
                                 Nothing -> ""
                                 Just False -> "transition-opacity opacity-0"
                                 Just True -> "transition-opacity"
                             )
                    ]
                    [pomodoroView (Just "text-neutral-400 font-semibold") i]
                futureItemView (i, idx) =
                  li_
                    [ class_ $
                        "px-2 "
                          <> ( case Map.lookup (FutureItemTransition idx) m._transitionMap of
                                 Nothing -> ""
                                 Just False -> "transition-[transform,opacity] -translate-y-16 opacity-0"
                                 Just True -> "transition-[transform,opacity]"
                             )
                    ]
                    [pomodoroView (Just "text-neutral-500 font-semibold text-lg") i]
             in [ case m._pomodoroPastQueue of
                    [] -> div_ [] []
                    justPast : rest -> ul_ [class_ "contents md:flex md:flex-col md:items-center md:justify-start md:self-start md:basis-1/4"] $ foldl' (\acc i -> pastItemView i : acc) [pastItemView justPast] rest,
                  currentView,
                  case m._pomodoroFutureQueue of
                    [] -> div_ [] []
                    nearFuture : rest ->
                      ul_
                        [class_ "contents md:flex md:flex-col md:items-center md:justify-end md:self-end md:basis-1/4"]
                        (futureItemView nearFuture : (futureItemView <$> rest))
                ]
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
          (Pomodoro.defaultCurrentPomodoro, PomodoroQueueIndex 0)
          Pomodoro.defaultPomodoroFutureQueue
          mempty
      )
      Pomodoro.updateModel
      Pomodoro.viewModel
  )
    { mailbox = \v -> case fromJSON v of
        Error _ -> Nothing
        Aeson.Success Clock.ClockDoneMessage -> Just Pomodoro.PreNextTransition
    }

pomodoroPRD :: ProductRequirementDocument
pomodoroPRD =
  ProductRequirementDocument
    ( ProblemAlignment
        ( Problem
            "A simple Pomodoro app"
            "I need some quick and simple on my site as a MVP to showcase my programming skills"
            "So that my site is not empty and it will be a tool that I could use"
            "Bonus It is a tool that people could use and potentially bring traffic"
            ""
            ""
            :| []
        )
        "Starting from features, evolving into a nice UI, don't overengineering it."
        ("A functional pomodoro timer" :| ["UI/UX should be simple and modern"])
    )
    ( SolutionAlignment
        "Settings -> Timer"
        [ "Users can set their desired time for pomodoro and breaks",
          "Users can start and stop the timer",
          "Users can fastforward to skip to the next stage"
        ]
        [ OpenIssues
            "Timer Settings Reasonable Values"
            $ "5 mins as the unit, I don't see we need more flexibility here, make it simple"
              :| [ "More than 5 mins, It is too fragment to do reasonable work or have a reasonable rest if the time is too short",
                   "Less than 45 mins, I don't think and human could sustain a undistrubed focus for that long while being productive",
                   "Following the tried and true 4 short break then a long break, I don't see the value of extra flexibilify as we offer to tweak the time for each stage already"
                 ]
        ]
        [Reference (Just "Pomofocus") [uri|https://pomofocus.io/|] $ "I don't need those complex features like it does but I like the simple and clean ui" :| []]
    )
    []
