{-# LANGUAGE DeriveAnyClass #-}

module Component.Clock (ClockDoneMessage (ClockDoneMessage), clockComponent) where

import Component.Clock.View
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Time
import GHC.Generics
import Miso
import Miso.JSON hiding ((.=))
import Miso.Lens
import Miso.String
import Prelude hiding (error)

active :: Lens Model Bool
active = lens _active $ \record x -> record {_active = x}

lastTick :: Lens Model (Maybe UTCTime)
lastTick = lens _lastTick $ \record x -> record {_lastTick = x}

timeLeft :: Lens Model NominalDiffTime
timeLeft = lens _timeLeft $ \record x -> record {_timeLeft = x}

data StopWatchTickSub = StopWatchTickSub

instance ToMisoString StopWatchTickSub where
  toMisoString _ = ms "StopWatchTickSub"

data ClockDoneMessage = ClockDoneMessage deriving (Eq, Generic, ToJSON, FromJSON)

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  End -> do
    stopSub StopWatchTickSub
    mailParent ClockDoneMessage
  Tick t ->
    use timeLeft >>= \case
      0 -> issue End
      timeleft' ->
        lastTick <<.= Just t >>= \case
          Nothing -> pure ()
          Just lt -> timeLeft .= max 0 (timeleft' - (t `diffUTCTime` lt))
  Start -> do
    active .= True
    startSub StopWatchTickSub $ \sink -> forever $ do
      liftIO $ threadDelay 100000
      liftIO getCurrentTime >>= sink . Tick
  Stop -> do
    active .= False
    lastTick .= Nothing
    stopSub StopWatchTickSub

clockComponent :: Bool -> NominalDiffTime -> Component parent Model Action
clockComponent active' currentPomodoroTime =
  (component (Model active' currentPomodoroTime Nothing) updateModel viewModel) {mount = Just Start}
