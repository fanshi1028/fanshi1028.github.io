{-# LANGUAGE OverloadedStrings #-}

module Route.View (routerView, routeToPRD) where

import Home
import Miso
import Miso.Html
import Miso.Html.Element
import Miso.Html.Property
import Miso.Svg.Element
import Miso.Svg.Property hiding (path_)
import Pomodoro
import ProductRequirementDocument
import Route

view500 :: MisoString -> View model action
view500 err =
  div_
    []
    [ text "TEMP FIXME: Internal Server Error 500",
      br_ [],
      text err
    ]

routeToPRD :: Route -> ProductRequirementDocument
routeToPRD = \case
  Index -> homePRD
  Pomodoro -> pomodoroPRD

routeToView :: Route -> View model Action
routeToView = \case
  Index -> home
  Pomodoro -> div_ [key_ @MisoString "pomodoro"] +> pomodoroComponent

homeButton Index = div_ [] []
homeButton _ =
  button_
    [ onClick $ GotoRoute Index,
      classes_
        [ "fixed z-50 hover:animate-wiggle hover:[animation-delay:0.25s]",
          "top-2 sm:top-4 md:top-6 lg:top-8 xl:top-12 2xl:top-16",
          "left-2 sm:left-4 md:left-6 lg:left-8 xl:left-12 2xl:left-16"
        ]
    ]
    [ svg_
        [ xmlns_ "http://www.w3.org/2000/svg",
          viewBox_ "0 0 24 24",
          classes_
            [ "fill-none stroke-2 stroke-neutral-600",
              "size-6 sm:size-8 md:size-10 lg:size-12 xl:size-16 2xl:size-20",
              "hover:size-8 sm:hover:size-10 md:hover:size-12 lg:hover:size-16 xl:hover:size-20 2xl:hover:size-24"
            ]
        ]
        [path_ [strokeLinecap_ "round", strokeLinejoin_ "round", d_ "M8.25 21v-4.875c0-.621.504-1.125 1.125-1.125h2.25c.621 0 1.125.504 1.125 1.125V21m0 0h4.5V3.545M12.75 21h7.5V10.75M2.25 21h1.5m18 0h-18M2.25 9l4.5-1.636M18.75 3l-1.5.545m0 6.205 3 1m1.5.5-1.5-.5M6.75 7.364V3h-3v18m3-13.636 10.5-3.819"]]
    ]

routerView :: (Either MisoString Route) -> View (Either MisoString Route) Action
routerView = \case
  Right route ->
    div_ [] $
      [ div_ [key_ @MisoString "prd"] +> (prdComponent False $ routeToPRD route),
        homeButton route,
        routeToView route
      ]
  Left err' -> view500 err'
