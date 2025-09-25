{-# LANGUAGE OverloadedStrings #-}

module Route.View (routerView) where

import Home
import Miso
import Miso.Html.Element
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

routerView :: (Either MisoString Route) -> View (Either MisoString Route) Action
routerView = \case
  Right route ->
    div_ [] $
      [ div_ [key_ @MisoString "prd"] +> (prdComponent False $ routeToPRD route),
        routeToView route
      ]
  Left err' -> view500 err'
