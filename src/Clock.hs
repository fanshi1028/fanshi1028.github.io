{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Clock (ClockDoneMessage (ClockDoneMessage), clockComponent) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding ((.=))
import Data.Fixed
import Data.Proxy
import Data.Time
import GHC.Float.RealFracMethods
import GHC.Generics
import Miso
import Miso.Html
import Miso.Html.Property
import Miso.Lens
import Miso.String
import Miso.Svg.Element
import Miso.Svg.Property hiding (path_)
import Prelude hiding (error)

data Model = Model
  { _active :: Bool,
    _timeLeft :: DiffTime,
    _lastTick :: Maybe Double
  }
  deriving (Eq)

active :: Lens Model Bool
active = lens _active $ \record x -> record {_active = x}

lastTick :: Lens Model (Maybe Double)
lastTick = lens _lastTick $ \record x -> record {_lastTick = x}

timeLeft :: Lens Model DiffTime
timeLeft = lens _timeLeft $ \record x -> record {_timeLeft = x}

data Action = Tick Double | Start | Stop | End

data StopWatchTickSub = StopWatchTickSub

instance ToMisoString StopWatchTickSub where
  toMisoString _ = "StopWatchTickSub"

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
          Just lt -> do
            -- NOTE: https://developer.mozilla.org/en-US/docs/Web/API/Performance/now
            let milliToPico = (* fromIntegral (resolution (Proxy @E12) `div` resolution (Proxy @E3)))
                diff = picosecondsToDiffTime . truncateDoubleInteger . milliToPico $ t - lt
            timeLeft .= max 0 (timeleft' - diff)
  Start -> do
    active .= True
    startSub StopWatchTickSub $ \sink -> forever $ do
      liftIO $ threadDelay 100000
      now >>= sink . Tick
  Stop -> do
    active .= False
    lastTick .= Nothing
    stopSub StopWatchTickSub

viewModel :: Model -> View Model Action
viewModel m =
  div_ [class_ "flex flex-col items-center justify-center gap-4 p-6 h-full"] $
    [ p_ [class_ "text-neutral-300 text-7xl sm:text-8xl font-mono font-semibold"] [text . ms . formatTime defaultTimeLocale "%M:%00ES" $ m ^. timeLeft],
      div_ [class_ "grid grid-cols-2 place-items-center w-full items-center"] $
        [ let (primaryAction, primarybuttonText) =
                if m ^. active
                  then (Stop, "Stop")
                  else (Start, "Start")
              buttonCls = "bg-neutral-300 text-neutral-600 text-3xl sm:text-4xl rounded px-4 sm:px-6 py-2 sm:py-3 shadow-inner shadow-neutral-800 hover:animate-wiggle"
           in button_ [onClick primaryAction, class_ buttonCls] [primarybuttonText],
          button_
            [onClick End, class_ "self-center w-fit mx-auto hover:animate-wiggle"]
            [ svg_
                [class_ "fill-neutral-300 size-16 sm:size-20", xmlns_ "http://www.w3.org/2000/svg", viewBox_ "0 0 24 24"]
                [path_ [d_ "M5.055 7.06C3.805 6.347 2.25 7.25 2.25 8.69v8.122c0 1.44 1.555 2.343 2.805 1.628L12 14.471v2.34c0 1.44 1.555 2.343 2.805 1.628l7.108-4.061c1.26-.72 1.26-2.536 0-3.256l-7.108-4.061C13.555 6.346 12 7.249 12 8.689v2.34L5.055 7.061Z"]]
            ]
        ]
    ]

clockComponent :: Bool -> DiffTime -> Component parent Model Action
clockComponent active' currentPomodoroTime =
  (component (Model active' currentPomodoroTime Nothing) updateModel viewModel) {initialAction = Just Clock.Start}
