{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Clock where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Fixed
import Data.Proxy
import Data.Time
import GHC.Float.RealFracMethods
import Miso
import Miso.Html
import Miso.Html.Property
import Miso.Lens
import Miso.String

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

data Action = Tick Double | Start | Stop

data StopWatchSub = StopWatchSub

instance ToMisoString StopWatchSub where
  toMisoString _ = "StopWatchSub"

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  Tick t -> do
    use lastTick >>= \case
      Nothing -> pure ()
      Just lt -> do
        -- NOTE: https://developer.mozilla.org/en-US/docs/Web/API/Performance/now
        let milliToPico = (* fromIntegral (resolution (Proxy @E12) `div` resolution (Proxy @E3)))
            diff = picosecondsToDiffTime . truncateDoubleInteger . milliToPico $ t - lt
        timeLeft %= max 0 . subtract diff
    lastTick .= Just t
  Start -> do
    active .= True
    startSub StopWatchSub $ \sink -> forever $ do
      liftIO $ threadDelay 100000
      now >>= sink . Tick
  Stop -> do
    active .= False
    lastTick .= Nothing
    stopSub StopWatchSub

viewModel :: Model -> View Model Action
viewModel m =
  div_ [class_ "flex flex-col items-center gap-6 p-6"] $
    let timeDisplay = text $ ms (formatTime defaultTimeLocale "%M:%00ES" $ m ^. timeLeft)
     in if m ^. timeLeft <= 0
          then [p_ [class_ "text-neutral-200"] ["Time's up"]]
          else
            if m ^. active
              then
                [ p_ [class_ "text-neutral-200 text-9xl"] [timeDisplay],
                  -- button_ [onClick Stop, class_ "text-neutral-200 text-4xl"] ["Stop"]
                  button_ [onClick Stop, class_ "bg-neutral-200 text-neutral-600 text-4xl rounded px-4 py-2"] ["Stop"]
                  -- div_
                  --   [class_ "flex flex-row justify-around w-full"]
                  --   [ button_ [onClick Stop, class_ "text-neutral-200 text-4xl rounded px-4 py-2"] ["Stop"],
                  --     button_ [onClick Stop, class_ "bg-neutral-200 text-neutral-600 text-4xl rounded px-4 py-2"] ["Stop"]
                  --   ]
                ]
              else
                [ p_ [class_ "text-neutral-200 text-9xl"] [timeDisplay],
                  -- button_ [onClick Start, class_ "text-neutral-200 text-4xl"] ["Start"]
                  button_ [onClick Start, class_ "bg-neutral-200 text-neutral-600 text-4xl rounded px-4 py-2 self-bew"] ["Start"]
                ]
