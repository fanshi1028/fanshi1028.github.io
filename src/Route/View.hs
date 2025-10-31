{-# LANGUAGE OverloadedStrings #-}

module Route.View (navView) where

import Dashboard
import Home
import Miso
import Miso.Html
import Miso.Html.Property
import Miso.Router qualified as Router
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

data UnderConstruction = UnderConstruction

routeToView :: Route -> Either UnderConstruction (View model Action)
routeToView = \case
  Index -> Right home
  Pomodoro -> Right $ div_ [key_ @MisoString "pomodoro"] +> pomodoroComponent
  Dashboard -> Right $ div_ [key_ @MisoString "dashboard"] +> dashboardComponent

homeButton :: Route -> View model Action
homeButton Index = div_ [] []
homeButton _ =
  a_
    [ onClickWithOptions preventDefault $ GotoRoute Index,
      Router.href_ Index,
      class_ "hover:animate-wiggle hover:[animation-delay:0.25s]"
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

prdButton :: Bool -> View model Action
prdButton open =
  button_
    [ onClick $ SetPRDOpen $ not open,
      class_ "hover:animate-wiggle hover:[animation-delay:0.25s]"
    ]
    [ svg_
        [ classes_
            [ "fill-none stroke-2 stroke-neutral-600",
              "size-6 sm:size-8 md:size-10 lg:size-12 xl:size-16 2xl:size-20",
              "hover:size-8 sm:hover:size-10 md:hover:size-12 lg:hover:size-16 xl:hover:size-20 2xl:hover:size-24"
            ],
          xmlns_ "http://www.w3.org/2000/svg",
          viewBox_ "0 0 24 24"
        ]
        [path_ [strokeLinecap_ "round", strokeLinejoin_ "round", d_ "M7.5 21 3 16.5m0 0L7.5 12M3 16.5h13.5m0-13.5L21 7.5m0 0L16.5 12M21 7.5H7.5"]]
    ]

navView :: Model -> View Model Action
navView = \case
  Model route open ->
    let navCls =
          classes_
            [ "fixed flex flex-col z-50",
              "gap-2 md:gap-4 xl:gap-6",
              "top-2 sm:top-4 md:top-6 lg:top-8 xl:top-12 2xl:top-16",
              "right-2 sm:right-4 md:right-6 lg:right-8 xl:right-12 2xl:right-16"
            ]
     in div_ [] $ case routeToView route of
          Left _ ->
            [ nav_ [navCls] [homeButton route],
              prdView True $ routeToPRD route
            ]
          Right vw ->
            [ nav_ [navCls] [homeButton route, prdButton open],
              prdView open $ routeToPRD route,
              vw
            ]
  RoutingError err' -> view500 err'
