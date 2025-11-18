{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module App (app, viewModel, Model (..), Action) where

import Control.Monad
import Dashboard
import Data.Char
import GHC.Enum
import GHC.Generics
import Language.Javascript.JSaddle
import Miso
import Miso.Html
import Miso.Html.Property
import Miso.Lens
import Miso.Router
import Miso.Router qualified as Router
import Miso.Svg.Element
import Miso.Svg.Property hiding (path_)
import Pomodoro
import ProductRequirementDocument
import ProductRequirementDocument.Dashboard
import ProductRequirementDocument.Home
import ProductRequirementDocument.Pomodoro
import Utils.SVG.LoadSpinner
import Utils.SVG.ToggleLangButton

newtype RouteForTheOtherLang = RouteForTheOtherLang Route

data Route
  = Index -- NOTE: Index must be the first one, code made assumpation base on its Enum being the first one.
  | Pomodoro
  | Dashboard
  deriving stock (Eq, Show, Enum, Bounded, Generic)
#ifndef wasm32_HOST_ARCH
  deriving anyclass (Router)

instance Router RouteForTheOtherLang where
  routeParser =
    routes
      [ path (ms "wasm") *> (RouteForTheOtherLang . to <$> gRouteParser),
        RouteForTheOtherLang Index <$ path (ms "wasm")
      ]
  fromRoute = \case
    RouteForTheOtherLang Index -> [ toPath $ ms "wasm" ]
    RouteForTheOtherLang route' -> toPath (ms "wasm") : gFromRoute (from route')
#endif

#ifdef wasm32_HOST_ARCH
instance Router Route where
  routeParser =
    routes
      [ path (ms "wasm") *> (to <$> gRouteParser),
        Index <$ path (ms "wasm")
      ]
  fromRoute = \case
    Index -> [ toPath $ ms "wasm" ]
    route' -> toPath (ms "wasm") : gFromRoute (from route')

instance Router RouteForTheOtherLang where
  routeParser = RouteForTheOtherLang . to <$> gRouteParser
  fromRoute (RouteForTheOtherLang route') = gFromRoute $ from route'
#endif

data Action
  = SetRoutingError RoutingError
  | GotoRoute Route
  | SetURI Route
  | SetPRDOpen Bool
  | AfterLoaded

data Model
  = RoutingError RoutingError
  | Model
      { _route :: Route,
        _loading :: Bool
      }
  deriving (Eq)

routeToPRD :: Route -> ProductRequirementDocument
routeToPRD = \case
  Index -> homePRD
  Pomodoro -> pomodoroPRD
  Dashboard -> dashboardPRD

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  SetRoutingError err -> this .= RoutingError err
  GotoRoute uri -> do
    io_ . pushURI $ toURI uri
    issue $ SetURI uri
  SetURI uri -> this .= Model uri False
  SetPRDOpen setOpen -> io_ . void $ do
    prdDialgoue <- getElementById prdDialogueId
    prdDialgoue # (if setOpen then "showModal" else "close") $ ()
  AfterLoaded ->
    get >>= \case
      RoutingError err -> pure ()
      Model uri _ -> put $ Model uri False

view500 :: Router.RoutingError -> View Model action
view500 err =
  div_
    []
    [ text "TEMP FIXME: Internal Server Error 500",
      br_ [],
      text . ms $ show err
    ]

homeButton :: View model Action
homeButton =
  a_
    [ onClickWithOptions preventDefault $ GotoRoute Index,
      Router.href_ Index,
      classes_
        [ "flex items-center justify-center",
          "hover:animate-wiggle hover:[animation-delay:0.25s]",
          "size-8 sm:size-10 md:size-12 lg:size-16 xl:size-20 2xl:size-24"
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

prdButton :: Bool -> Bool -> View model Action
prdButton loading setOpen =
  button_
    [ onClick $ SetPRDOpen setOpen,
      classes_ $
        (if loading then "pointer-events-none animate-pulse" else "hover:animate-wiggle hover:[animation-delay:0.25s]")
          : [ "flex items-center justify-center",
              "size-8 sm:size-10 md:size-12 lg:size-16 xl:size-20 2xl:size-24"
            ]
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

toggleLangButton :: Bool -> Bool -> Route -> View model Action
toggleLangButton useWASM' loading route' =
  a_
    [ Router.href_ $ RouteForTheOtherLang route',
      classes_
        [ "flex items-center",
          "hover:animate-wiggle hover:[animation-delay:0.25s]",
          "size-8 sm:size-10 md:size-12 lg:size-16 xl:size-20 2xl:size-24"
        ]
    ]
    [if useWASM' then toJsButtonSVG else toWasmButtonSVG]

viewModel :: Bool -> Model -> View Model Action
viewModel useWASM' = \case
  RoutingError err' -> view500 err'
  Model route' loading
    | route' `elem` underConstruction -> div_ [] [prdView False (div_ [dialogButtonClss] [homeButton]) $ routeToPRD route']
    | otherwise ->
        div_ [] $
          [ nav_ [classes_ $ "fixed flex flex-col z-50 md:gap-2 xl:gap-4" : topRightClss] $ case route' of
              Index -> [toggleLangButton useWASM' loading route', prdButton loading True]
              _ -> [homeButton, toggleLangButton useWASM' loading route', prdButton loading True],
            prdView True (div_ [dialogButtonClss] [prdButton loading False]) $ routeToPRD route',
            case route' of
              Index ->
                div_
                  [class_ "flex flex-col items-center justify-center min-h-dvh bg-neutral-200"]
                  [ h1_ [class_ "sr-only"] [text $ if loading then "Tools (loading)" else "Tools"],
                    ul_
                      [ classes_
                          [ "flex flex-col",
                            "gap-4 sm:gap-5 md:gap-6 lg:gap-8 xl:gap-10 2xl:gap-12"
                          ]
                      ]
                      $ ( \nonHomeRoute ->
                            button_
                              [ onClick $ GotoRoute nonHomeRoute,
                                classes_
                                  [ if loading then "pointer-events-none animate-pulse relative" else " hover:animate-wiggle",
                                    "font-bold text-neutral-700 bg-neutral-400 shadow-md shadow-neutral-600 rounded-lg",
                                    "text-lg sm:text-xl md:text-2xl lg:text-3xl xl:text-4xl 2xl:text-5xl",
                                    "px-4 sm:px-6 md:px-8 lg:px-10 xl:px-12 2xl:px-14"
                                  ]
                              ]
                              [ if loading
                                  then
                                    loadSpinner
                                      [ "size-6 sm:size-8 md:size-10 lg:size-12 xl:size-16 2xl:size-20",
                                        "absolute inset-y-2.5 sm:inset-y-3 md:inset-y-3.5 lg:inset-y-5 xl:inset-y-5.5 2xl:inset-y-6"
                                      ]
                                  else div_ [class_ "hidden"] [],
                                div_ [classes_ ["my-2 sm:my-3 md:my-4 lg:my-6 xl:my-8 2xl:my-10"]] [text . ms $ toUpper <$> show nonHomeRoute]
                              ]
                        )
                        <$> boundedEnumFrom (succ minBound) -- NOTE: exclude Index
                  ]
              Pomodoro -> div_ [key_ @MisoString "pomodoro"] +> pomodoroComponent
              Dashboard -> div_ [key_ @MisoString "dashboard"] +> dashboardComponent
          ]
  where
    topRightClss =
      [ "top-2 sm:top-4 md:top-6 lg:top-8 xl:top-12 2xl:top-16",
        "right-2 sm:right-4 md:right-6 lg:right-8 xl:right-12 2xl:right-16"
      ]
    dialogButtonClss = classes_ $ "sticky self-end z-50" : topRightClss
    underConstruction = []

useWASM :: Bool
#ifdef wasm32_HOST_ARCH
useWASM = True
#endif
#ifndef wasm32_HOST_ARCH
useWASM = False
#endif

app :: URI -> Component parent Model Action
app route' =
  (component model updateModel $ viewModel useWASM)
    { subs =
        [ routerSub $ \case
            Left err -> SetRoutingError err
            Right uri' -> SetURI uri'
        ],
#ifndef PRODUCTION
      scripts = [Src "https://cdn.tailwindcss.com"],
      styles = [Href "static/input.css"],
      logLevel = DebugAll,
#endif
      initialAction = Just AfterLoaded
    }
  where
    model = case route route' of
      Left err -> RoutingError err
      Right uri' -> Model uri' True
