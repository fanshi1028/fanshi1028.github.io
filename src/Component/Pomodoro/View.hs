{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Component.Pomodoro.View
  ( Model (..),
    Action (..),
    Pomodoro (..),
    PomodoroStage (..),
    PomodoroQueueIndex (..),
    PomodoroSettings (..),
    PomodoroMinuteSettingValidationError (..),
    ValueWithValidation (..),
    viewModel,
    defaultModel,
    pomodoro,
    shortBreak,
    longBreak,
    stageToSettingLens,
    settings,
    pomodoroPastQueue,
  )
where

import Component.Clock
import Control.Applicative
import Control.Category
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Time
import GHC.Natural
import Miso
import Miso.CSS hiding (ms, rem)
import Miso.Html as HTML
import Miso.Html.Property as P
import Miso.Lens
import Miso.String hiding (foldl')
import Miso.Svg.Element
import Miso.Svg.Property hiding (max_, min_, path_)
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
    _pomodoroTime :: NominalDiffTime
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

data Model = Model
  { _settings :: PomodoroSettings,
    _settingsOpen :: Bool,
    _pomodoroPastQueues :: NonEmpty [(Pomodoro, PomodoroQueueIndex)],
    _pomodoroQueue :: [(Pomodoro, PomodoroQueueIndex)]
  }
  deriving (Eq)

settings :: Lens Model PomodoroSettings
settings = lens _settings $ \record x -> record {_settings = x}

pomodoroPastQueue :: Lens Model [(Pomodoro, PomodoroQueueIndex)]
pomodoroPastQueue = lens (NE.head . _pomodoroPastQueues) $ \record x -> record {_pomodoroPastQueues = x :| NE.tail record._pomodoroPastQueues}

data Action
  = SettingsOpen Bool
  | Set PomodoroStage MisoString
  | ApplyPomodoroSettings
  | Next
  | PomodoroEnd
  deriving stock (Eq, Show)

viewModel :: Model -> View Model Action
viewModel m = div_ [class_ "flex flex-col container items-center lg:justify-center min-h-dvh bg-neutral-200 mx-auto gap-6 sm:gap-8 p-16 sm:p-20"] $ case m._pomodoroQueue of
  [] ->
    [ div_ [] $ case m ^. pomodoroPastQueue of
        [] -> []
        _ ->
          [ h1_ [class_ "text-center text-lg sm:text-xl text-neutral-600 font-bold"] [text "Pomodoro Ended!"],
            p_ [class_ "text-center sm:text-lg text-neutral-600"] [text "Ready to start a new Pomodoro?"]
          ],
      div_ [class_ sizeCls] $ [settingsView True]
    ]
  current@(_pomodoroTime -> currentPomodoroTime, _) : future ->
    [ h1_ [class_ "sr-only"] [text "Pomodoro"],
      let currentPomodoroView =
            div_
              [ classes_ ["absolute transition-transform duration-500 bg-neutral-600 rounded-lg shadow-lg shadow-neutral-600", sizeCls],
                styleInline_ $ "backface-visibility:hidden;" <> if m._settingsOpen then "transform:rotateY(180deg);" else ""
              ]
              [ button_
                  [onClick $ SettingsOpen True, class_ "absolute top-4 sm:top-4 right-4 sm:right-4 hover:animate-wiggle"]
                  [ p_ [class_ "sr-only"] ["Open Settings"],
                    svg_
                      [class_ "fill-none stroke-2 stroke-neutral-400 size-8 sm:size-10 hover:size-10 sm:hover:size-12", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 24 24"]
                      [ path_ [strokeLinecap_ "round", strokeLinejoin_ "round", d_ "M9.594 3.94c.09-.542.56-.94 1.11-.94h2.593c.55 0 1.02.398 1.11.94l.213 1.281c.063.374.313.686.645.87.074.04.147.083.22.127.325.196.72.257 1.075.124l1.217-.456a1.125 1.125 0 0 1 1.37.49l1.296 2.247a1.125 1.125 0 0 1-.26 1.431l-1.003.827c-.293.241-.438.613-.43.992a7.723 7.723 0 0 1 0 .255c-.008.378.137.75.43.991l1.004.827c.424.35.534.955.26 1.43l-1.298 2.247a1.125 1.125 0 0 1-1.369.491l-1.217-.456c-.355-.133-.75-.072-1.076.124a6.47 6.47 0 0 1-.22.128c-.331.183-.581.495-.644.869l-.213 1.281c-.09.543-.56.94-1.11.94h-2.594c-.55 0-1.019-.398-1.11-.94l-.213-1.281c-.062-.374-.312-.686-.644-.87a6.52 6.52 0 0 1-.22-.127c-.325-.196-.72-.257-1.076-.124l-1.217.456a1.125 1.125 0 0 1-1.369-.49l-1.297-2.247a1.125 1.125 0 0 1 .26-1.431l1.004-.827c.292-.24.437-.613.43-.991a6.932 6.932 0 0 1 0-.255c.007-.38-.138-.751-.43-.992l-1.004-.827a1.125 1.125 0 0 1-.26-1.43l1.297-2.247a1.125 1.125 0 0 1 1.37-.491l1.216.456c.356.133.751.072 1.076-.124.072-.044.146-.086.22-.128.332-.183.582-.495.644-.869l.214-1.28Z"],
                        path_ [strokeLinecap_ "round", strokeLinejoin_ "round", d_ "M15 12a3 3 0 1 1-6 0 3 3 0 0 1 6 0Z"]
                      ]
                  ],
                div_
                  [class_ "h-full w-full pt-6"]
                  [ "stopwatch " <> ms (show currentPomodoroTime) +> clockComponent False currentPomodoroTime
                  ]
              ]
       in div_
            [class_ "contents"]
            [ h2_ [class_ "sr-only"] [text "Pomodoro Queue"],
              ul_ [class_ "flex flex-col lg:flex-row lg:justify-around lg:w-full items-center gap-3"] $
                let pastItemView (i, idx) =
                      li_
                        [ key_ $ "past-item-" <> show idx,
                          classes_
                            [ "px-2",
                              "transition-[transform,opacity] opacity-100 starting:opacity-0 translate-y-0 starting:translate-y-16"
                            ]
                        ]
                        [pomodoroView (Just "text-neutral-400 font-semibold sm:text-lg xl:text-xl") i]
                    futureItemView extraCls (i, idx) =
                      li_
                        [key_ $ "future-item-" <> show idx, classes_ ["px-2", extraCls]]
                        [pomodoroView (Just "text-neutral-500 font-semibold text-lg sm:text-xl xl:text-2xl") i]
                 in [ case m ^. pomodoroPastQueue of
                        [] -> div_ [class_ "lg:basis-1/4"] []
                        justPast : rest -> ul_ [class_ "contents md:flex md:flex-col md:items-center md:justify-start md:self-start lg:basis-1/4 xl:gap-2"] $ foldl' (\acc i -> pastItemView i : acc) [pastItemView justPast] rest,
                      div_ [classes_ ["relative my-6", sizeCls]] [settingsView False, currentPomodoroView],
                      ul_
                        [class_ "contents md:flex md:flex-col md:items-center md:justify-end md:self-end lg:basis-1/4 gap-2 xl:gap-4"]
                        (futureItemView "transition-[transform,opacity] opacity-0 -translate-y-16 h-0" current : (futureItemView "" <$> future))
                    ]
            ]
    ]
  where
    sizeCls = "w-80 sm:w-96 h-64 sm:h-80"
    pomodoroView mCls (MkPomodoro stage time) =
      div_ [classes_ ["flex flex-row gap-4", fromMaybe "" mCls]] $
        [ p_ [] [text $ stageToMisoString stage <> ": "],
          span_ [] [text . ms $ formatTime defaultTimeLocale "%M:%00ES" time]
        ]

    settingView stage =
      let v = m ^. settings . (stageToSettingLens stage)
          disableIfOtherInvalidated = case traverse (\stage' -> (m ^. settings . (stageToSettingLens stage'))._validation) [minBound .. maxBound] of
            Validation.Success _ -> Prelude.id
            Failure _ -> case v._validation of
              Validation.Success _ -> (disabled_ :)
              Failure _ -> Prelude.id
          stageNameInputId = stageToMisoString stage <> " mins"
       in div_
            [class_ "w-4/5 has-[:invalid]:w-full flex flex-col-reverse gap-2 has-[:invalid]:grid has-[:invalid]:grid-rows-2 has-[:invalid]:grid-cols-12 has-[:invalid]:col-span-2 has-[:invalid]:row-start-1 has-[:invalid]:mt-6 sm:has-[:invalid]:mt-12"]
            [ input_
                ( disableIfOtherInvalidated
                    [ class_ "peer bg-neutral-300 text-neutral-600 text-lg font-bold rounded focus:ring-0 focus:border-0 focus:outline-1 focus:outline-neutral-800 w-full shadow-inner shadow-neutral-800 invalid:col-span-7 invalid:col-start-3 invalid:text-xl disabled:text-neutral-400",
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
              HTML.label_ [class_ "text-neutral-400 font-semibold sm:text-lg peer-invalid:[writing-mode:sideways-lr] peer-invalid:col-span-2 peer-invalid:col-start-1 peer-invalid:row-start-1 peer-invalid:row-span-2 peer-invalid:text-lg sm:peer-invalid:text-xl peer-invalid:tracking-tight peer-invalid:text-right peer-invalid:ml-2 sm;peer-invalid:ml-4", for_ stageNameInputId] [text $ stageToMisoString stage],
              case v._validation of
                Validation.Success _ -> div_ [] []
                Failure (toList -> errs) ->
                  ul_ [class_ "list-disc list-inside invisible peer-invalid:visible peer-invalid:col-span-9 peer-invalid:row-start-2 peer-invalid:col-start-3"] $
                    li_ [class_ "text-neutral-200 font-semibold leading-tight"] . (: []) . text . unPomodoroMinuteSettingValidationError <$> errs
            ]

    settingsView init' =
      div_
        [ classes_ ["absolute transition-transform duration-500 rounded-lg shadow-lg bg-neutral-600 shadow-neutral-600 pb-4 has-[:invalid]:pb-0", sizeCls],
          styleInline_ $ "backface-visibility:hidden;" <> if m._settingsOpen then "" else "transform:rotateY(180deg);"
        ]
        $ [ h2_ [class_ "sr-only"] ["Settings"],
            div_
              [class_ "group grid grid-rows-2 grid-cols-2 place-items-center w-full h-full p-4"]
              [ div_ [class_ "contents"] $ settingView <$> [minBound .. maxBound],
                div_ [class_ "flex flex-row justify-around"] $
                  [ button_
                      [onClick ApplyPomodoroSettings, class_ "group-[:has(:invalid)]:hidden hover:animate-wiggle hover:[animation-delay:0.25s]"]
                      [ p_ [class_ "sr-only"] ["Apply"],
                        svg_
                          [class_ "fill-none stroke-2 stroke-neutral-400 size-12 sm:size-14 hover:size-14 sm:hover:size-16", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 24 24"]
                          [path_ [strokeLinecap_ "round", strokeLinejoin_ "round", d_ "m4.5 12.75 6 6 9-13.5"]]
                      ],
                    if init'
                      then
                        div_ [] []
                      else
                        button_
                          [onClick $ SettingsOpen False, class_ "group-[:has(:invalid)]:absolute top-2 sm:top-4 right-2 sm:right-4 hover:animate-wiggle"]
                          [ p_ [class_ "sr-only"] ["Close"],
                            svg_
                              [class_ "fill-none stroke-2 stroke-neutral-400 size-12 sm:size-14 hover:size-14 sm:hover:size-16", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 24 24"]
                              [path_ [strokeLinecap_ "round", strokeLinejoin_ "round", d_ "M6 18 18 6M6 6l12 12"]]
                          ]
                  ]
              ]
          ]

defaultPomodoro, defaultShortBreak, defaultLongBreak :: Natural
defaultLongBreak = 15
defaultShortBreak = 5
defaultPomodoro = 25

defaultModel :: Model
defaultModel =
  Model
    ( PomodoroSettings
        (ValueWithValidation (ms $ show defaultPomodoro) $ pure defaultPomodoro)
        (ValueWithValidation (ms $ show defaultShortBreak) $ pure defaultShortBreak)
        (ValueWithValidation (ms $ show defaultLongBreak) $ pure defaultLongBreak)
    )
    True
    ([] :| [])
    []
