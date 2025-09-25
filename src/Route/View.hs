{-# LANGUAGE OverloadedStrings #-}

module Route.View where

import Home
import Miso
import Miso.Html.Element
import Pomodoro
import Route

view500 :: MisoString -> View model action
view500 err =
  div_
    []
    [ text "TEMP FIXME: Internal Server Error 500",
      br_ [],
      text err
    ]

routeToView :: (Either MisoString Route) -> View (Either MisoString Route) Action
routeToView = \case
  Right Index -> home
  Right Pomodoro -> div_ [key_ @MisoString "pomodoro-app"] +> pomodoroApp
  Left err' -> view500 err'
