{-# LANGUAGE CPP #-}
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

view500 :: Router.RoutingError -> View Model action
view500 err =
  div_
    []
    [ text "TEMP FIXME: Internal Server Error 500",
      br_ [],
      text . ms $ show err
    ]

data UnderConstruction = UnderConstruction

routeToView :: Route -> Either UnderConstruction (View Model Action)
routeToView = \case
  Index -> Right home
  Pomodoro -> Right $ div_ [key_ @MisoString "pomodoro"] +> pomodoroComponent
  Dashboard -> Right $ div_ [key_ @MisoString "dashboard"] +> dashboardComponent

homeButton :: Bool -> View model Action
homeButton loading =
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

prdButton :: Bool -> Bool -> View model Action
prdButton loading setOpen =
  button_
    [ onClick $ SetPRDOpen setOpen,
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

toggleWASMButton :: Bool -> Route -> View model Action
toggleWASMButton loading route =
  a_
    [ Router.href_ $ ToggleWASM route,
      class_ "hover:animate-wiggle hover:[animation-delay:0.25s]"
    ]
    [ svg_
        [ classes_
            [
#ifdef wasm32_HOST_ARCH
              "bg-neutral-600",
#endif
              "size-6 sm:size-8 md:size-10 lg:size-12 xl:size-16 2xl:size-20",
              "hover:size-8 sm:hover:size-10 md:hover:size-12 lg:hover:size-16 xl:hover:size-20 2xl:hover:size-24"
            ],
          xmlns_ "http://www.w3.org/2000/svg",
          viewBox_ "-12 0 36 24"
        ]
        [ path_
            [
#ifndef wasm32_HOST_ARCH
              class_ "fill-[#654FF0]",
              d_ "M14.745,0c0,0.042,0,0.085,0,0.129c0,1.52-1.232,2.752-2.752,2.752c-1.52,0-2.752-1.232-2.752-2.752 c0-0.045,0-0.087,0-0.129H0v24h24V0H14.745z M11.454,21.431l-1.169-5.783h-0.02l-1.264,5.783H7.39l-1.824-8.497h1.59l1.088,5.783 h0.02l1.311-5.783h1.487l1.177,5.854h0.02l1.242-5.854h1.561l-2.027,8.497H11.454z M20.209,21.431l-0.542-1.891h-2.861l-0.417,1.891 h-1.59l2.056-8.497h2.509l2.5,8.497H20.209z M17.812,15.028l-0.694,3.118h2.159l-0.796-3.118H17.812z"
#endif
#ifdef wasm32_HOST_ARCH
              class_ "fill-[#F7DF1E]",
              d_ "M0 0h24v24H0V0zm22.034 18.276c-.175-1.095-.888-2.015-3.003-2.873-.736-.345-1.554-.585-1.797-1.14-.091-.33-.105-.51-.046-.705.15-.646.915-.84 1.515-.66.39.12.75.42.976.9 1.034-.676 1.034-.676 1.755-1.125-.27-.42-.404-.601-.586-.78-.63-.705-1.469-1.065-2.834-1.034l-.705.089c-.676.165-1.32.525-1.71 1.005-1.14 1.291-.811 3.541.569 4.471 1.365 1.02 3.361 1.244 3.616 2.205.24 1.17-.87 1.545-1.966 1.41-.811-.18-1.26-.586-1.755-1.336l-1.83 1.051c.21.48.45.689.81 1.109 1.74 1.756 6.09 1.666 6.871-1.004.029-.09.24-.705.074-1.65l.046.067zm-8.983-7.245h-2.248c0 1.938-.009 3.864-.009 5.805 0 1.232.063 2.363-.138 2.711-.33.689-1.18.601-1.566.48-.396-.196-.597-.466-.83-.855-.063-.105-.11-.196-.127-.196l-1.825 1.125c.305.63.75 1.172 1.324 1.517.855.51 2.004.675 3.207.405.783-.226 1.458-.691 1.811-1.411.51-.93.402-2.07.397-3.346.012-2.054 0-4.109 0-6.179l.004-.056z"
#endif
            ],
          path_
            [ classes_
                [ "stroke-2",
#ifndef wasm32_HOST_ARCH
                  "stroke-purple-400"
#endif
#ifdef wasm32_HOST_ARCH
                  "stroke-orange-600"
#endif
                ],
              transform_ "translate(-12,0)",
              strokeLinecap_ "round",
              strokeLinejoin_ "round",
              d_ "M7.5 21 3 16.5m0 0L7.5 12M3 16.5h13.5m0-13.5L21 7.5m0 0L16.5 12M21 7.5H7.5"
            ]
        ]
    ]

topRightClss :: [MisoString]
topRightClss =
  [ "top-2 sm:top-4 md:top-6 lg:top-8 xl:top-12 2xl:top-16",
    "right-2 sm:right-4 md:right-6 lg:right-8 xl:right-12 2xl:right-16"
  ]

navView :: Model -> View Model Action
navView = \case
  Model route loading ->
    let navCls = classes_ $ "fixed flex flex-col z-50 gap-2 md:gap-4 xl:gap-6" : topRightClss
        dialogButtonClss = classes_ $ "sticky self-end z-50" : topRightClss
     in div_ [] $ case routeToView route of
          Left UnderConstruction -> [prdView False (div_ [dialogButtonClss] [homeButton loading]) $ routeToPRD route]
          Right vw ->
            [ nav_ [navCls] $ case route of
                Index -> [toggleWASMButton loading route, prdButton loading True]
                _ -> [homeButton loading, toggleWASMButton loading route, prdButton loading True],
              prdView True (div_ [dialogButtonClss] [prdButton loading False]) $ routeToPRD route,
              vw
            ]
  RoutingError err' -> view500 err'
