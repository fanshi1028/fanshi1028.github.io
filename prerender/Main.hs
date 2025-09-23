{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Miso
import Miso.Html.Element as Html
import Miso.Html.Property
import Miso.Html.Render
import qualified Pomodoro
import ProductRequirementDocument as PRD
import System.File.OsPath as IO
import System.OsPath

main :: IO ()
main =
  IO.writeFile [osp|index.html|] . toHtml $
    wrapHtml . div_ [] $
      [ div_ [key_ @MisoString "prd"]
          +> ( component
                 (PRD.Model False Pomodoro.pomodoroPRD)
                 PRD.updateModel
                 PRD.viewModel
             )
            { initialAction = Just PRD.Subscribe
            },
        div_ [key_ @MisoString "pomodoro"] +> Pomodoro.pomodoroComponent
      ]

wrapHtml :: View model action -> [View model action]
wrapHtml innerView =
  [ doctype_,
    html_
      []
      [ head_
          []
          [ meta_ [charset_ "utf-8"],
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"],
            Html.title_ [] ["Fanshi1028's personal site"],
            link_ [href_ "output.css", rel_ "stylesheet", type_ "text/css"]
          ],
        body_ [] $
          [ script_ [src_ "index.js", type_ "module"] "",
            innerView
          ]
      ]
  ]
