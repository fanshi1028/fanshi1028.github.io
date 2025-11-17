{-# LANGUAGE OverloadedStrings #-}

module Utils.SVG.LoadSpinner where

import Miso
import Miso.Html
import Miso.Html.Property
import Miso.Svg.Element
import Miso.Svg.Property hiding (path_)

loadSpinner :: [MisoString] -> View model action
loadSpinner clss =
  svg_
    [ classes_ $ "animate-spin" : clss,
      xmlns_ "http://www.w3.org/2000/svg",
      viewBox_ "0 0 12 12"
    ]
    [ circle_
        [ class_ "fill-transparent stroke-2 stroke-neutral-200",
          cx_ "6",
          cy_ "6",
          r_ "5",
          strokeLinecap_ "round",
          strokeDasharray_ "12",
          strokeDashoffset_ "2",
          transformOrigin_ "50% 50%"
        ]
    ]
