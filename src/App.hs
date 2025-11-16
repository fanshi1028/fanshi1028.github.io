{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module App (app, viewModel) where

import App.Types
import Control.Monad
import Dashboard
import Home
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
import Utils.SVG.ToggleLangButton

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
  SetURI uri -> this .= Model uri True
  SetPRDOpen setOpen -> io_ . void $ do
    prdDialgoue <- getElementById prdDialogueId
    prdDialgoue # (if setOpen then "showModal" else "close") $ ()
  AfterLoaded ->
    get >>= \case
      RoutingError err -> pure ()
      Model uri _ -> put $ Model uri False

-- Model uri _ -> pure () -- TEMP FIXME

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
toggleWASMButton loading route' =
  a_
    [ Router.href_ $ ToggleWASM route',
      class_ "hover:animate-wiggle hover:[animation-delay:0.25s]"
    ]
    [toggleLangButtonSVG]

topRightClss :: [MisoString]
topRightClss =
  [ "top-2 sm:top-4 md:top-6 lg:top-8 xl:top-12 2xl:top-16",
    "right-2 sm:right-4 md:right-6 lg:right-8 xl:right-12 2xl:right-16"
  ]

viewModel :: Model -> View Model Action
viewModel = \case
  Model route' loading ->
    let navCls = classes_ $ "fixed flex flex-col z-50 gap-2 md:gap-4 xl:gap-6" : topRightClss
        dialogButtonClss = classes_ $ "sticky self-end z-50" : topRightClss
     in div_ [] $ case routeToView route' of
          Left UnderConstruction -> [prdView False (div_ [dialogButtonClss] [homeButton loading]) $ routeToPRD route']
          Right vw ->
            [ nav_ [navCls] $ case route' of
                Index -> [toggleWASMButton loading route', prdButton loading True]
                _ -> [homeButton loading, toggleWASMButton loading route', prdButton loading True],
              prdView True (div_ [dialogButtonClss] [prdButton loading False]) $ routeToPRD route',
              vw
            ]
  RoutingError err' -> view500 err'

app :: URI -> Component parent Model Action
app route' =
  (component model updateModel viewModel)
    { 
#ifndef PRODUCTION
      scripts = [Src "https://cdn.tailwindcss.com"],
      styles = [Href "static/input.css"],
      logLevel = DebugAll,
#endif
      subs =
        [ routerSub $ \case
            Left err -> SetRoutingError err
            Right uri' -> SetURI uri'
        ],
      initialAction = Just AfterLoaded
    }
  where
    model = case route route' of
      Left err -> RoutingError err
      Right uri' -> Model uri' True
