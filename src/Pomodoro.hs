{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Pomodoro (pomodoroComponent) where

import Clock
import Control.Applicative
import Control.Category
import Control.Monad
import Data.Aeson as Aeson hiding ((.=))
import Data.Bifunctor
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.Time
import GHC.Generics
import GHC.Natural
import Miso hiding (Transition)
import Miso.Lens
import Miso.String hiding (foldl')
import Pomodoro.View
import Text.Read
import Validation as Validation hiding (validation)
import Prelude hiding ((.))

value :: Lens ValueWithValidation MisoString
value = lens _value $ \record x -> record {_value = x}

validation :: Lens ValueWithValidation (Validation (NonEmpty PomodoroMinuteSettingValidationError) Natural)
validation = lens _validation $ \record x -> record {_validation = x}

settingsOpen :: Lens Model Bool
settingsOpen = lens _settingsOpen $ \record x -> record {_settingsOpen = x}

pomodoroAllPastQueues :: Lens Model (NonEmpty [(Pomodoro, PomodoroQueueIndex)])
pomodoroAllPastQueues = lens _pomodoroPastQueues $ \record x -> record {_pomodoroPastQueues = x}

pomodoroQueue :: Lens Model [(Pomodoro, PomodoroQueueIndex)]
pomodoroQueue = lens _pomodoroQueue $ \record x -> record {_pomodoroQueue = x}

naturalAsMinutesToDiffTime :: Natural -> NominalDiffTime
naturalAsMinutesToDiffTime n = realToFrac . secondsToDiffTime $ 60 * fromIntegral n

mkDefaultPomodoroQueue :: NominalDiffTime -> NominalDiffTime -> NominalDiffTime -> [(Pomodoro, PomodoroQueueIndex)]
mkDefaultPomodoroQueue pomodoro' shortBreak' longBreak' =
  Prelude.zip
    [ MkPomodoro Pomodoro pomodoro',
      MkPomodoro ShortBreak shortBreak',
      MkPomodoro Pomodoro pomodoro',
      MkPomodoro ShortBreak shortBreak',
      MkPomodoro Pomodoro pomodoro',
      MkPomodoro ShortBreak shortBreak',
      MkPomodoro Pomodoro pomodoro',
      MkPomodoro LongBreak longBreak'
    ]
    $ PomodoroQueueIndex <$> [0 ..]

data PomodoroDone = PomodoroDone
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

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
          ((== []) -> noFuture) <-
            pomodoroQueue
              <<%= \case
                [] -> mkDefaultPomodoroQueue pomodoro'' shortBreak'' longBreak''
                current : future ->
                  current
                    : ( first
                          ( \(MkPomodoro stage _) -> case stage of
                              LongBreak -> MkPomodoro stage longBreak''
                              ShortBreak -> MkPomodoro stage shortBreak''
                              Pomodoro -> MkPomodoro stage pomodoro''
                          )
                          <$> future
                      )
          when noFuture $ pomodoroAllPastQueues %= ([] <|)
          settingsOpen .= False
  SettingsOpen open -> settingsOpen .= open
  Next ->
    use pomodoroQueue >>= \case
      [] -> issue PomodoroEnd
      current : restFuture -> do
        case restFuture of
          [] -> settingsOpen .= True
          _ -> pure ()
        pomodoroPastQueue %= (current :)
        pomodoroQueue .= restFuture
  PomodoroEnd -> mailParent PomodoroDone
  Set (stageToSettingLens -> stageLens) str -> do
    let validateMax45 n = failureIf (n > 45) (PomodoroMinuteSettingValidationError "must <= 45")
        validateMin5 n = failureIf (n < 5) (PomodoroMinuteSettingValidationError "must >= 5")
        validateMultiple5 n = failureIf (n `rem` 5 /= 0) (PomodoroMinuteSettingValidationError "must be a multiple of 5")
    value . stageLens . settings .= str
    validation . stageLens . settings .= case readEither $ fromMisoString str of
      Left _ -> failure (PomodoroMinuteSettingValidationError $ "must be a number")
      Right n -> validateAll [validateMultiple5, validateMin5, validateMax45] n

pomodoroComponent :: Component parent Model Action
pomodoroComponent =
  (component defaultModel updateModel viewModel)
    { mailbox = \v -> case fromJSON v of
        Error _ -> Nothing
        Aeson.Success Clock.ClockDoneMessage -> Just Next
    }
