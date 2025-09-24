{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Home
import Miso
import Miso.Html.Element as Html
import Miso.Html.Property
import Miso.Html.Render
import Pomodoro
import System.File.OsPath as IO
import System.OsPath

main :: IO ()
main = do
  IO.writeFile [osp|index.html|] . toHtml $ wrapHtml home
  IO.writeFile [osp|pomodoro.html|] . toHtml $ wrapHtml pomodoroApp

wrapHtml :: App model action -> [View model action]
wrapHtml app =
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
            app.view app.model
          ]
      ]
  ]
