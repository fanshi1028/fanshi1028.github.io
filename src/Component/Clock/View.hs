{-# LANGUAGE OverloadedStrings #-}

module Component.Clock.View (viewModel, Model (..), Action (..)) where

import Data.Time
import Miso
import Miso.Html
import Miso.Html.Property
import Miso.Svg.Element
import Miso.Svg.Property hiding (path_)

data Model = Model
  { _active :: Bool,
    _timeLeft :: NominalDiffTime,
    _lastTick :: Maybe UTCTime
  }
  deriving (Eq)

data Action = Tick UTCTime | Start | Stop | End

viewModel :: Model -> View Model Action
viewModel (Model active timeLeft _) =
  div_ [class_ "flex flex-col items-center justify-center gap-4 p-6 h-full"] $
    [ p_ [class_ "text-neutral-300 text-7xl sm:text-8xl font-mono font-semibold"] [text . ms $ formatTime defaultTimeLocale "%M:%00ES" timeLeft],
      div_ [class_ "grid grid-cols-2 place-items-center w-full items-center"] $
        [ let (primaryAction, primarybuttonText) =
                if active
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
