{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Char
import Data.Foldable
import GHC.Enum
import Miso
import Miso.Html.Element as Html
import Miso.Html.Property
import Miso.Html.Render
import Route
import System.File.OsPath as IO
import System.OsPath

main :: IO ()
main =
  traverse_
    ( \route -> do
        file <- (<.>) <$> encodeUtf (toLower <$> show route) <*> encodeUtf ".html"
        IO.writeFile file . toHtml . routeToView $ Right route
    )
    $ boundedEnumFrom minBound

wrapHtml :: View model action -> [View model action]
wrapHtml vw =
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
            vw
          ]
      ]
  ]
