{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import App
import Data.ByteString.Lazy as BS
import Data.Foldable
import Data.Function
import GHC.Enum
import Miso
import Miso.Html.Element as Html
import Miso.Html.Property
import Miso.Html.Render
import Miso.Router hiding (href_)
import System.File.OsPath as IO
import System.IO
import System.OsPath

#ifdef WASM
import System.Directory.OsPath
#endif

createWasmDirIfMissing :: IO ()
#ifdef WASM
createWasmDirIfMissing = createDirectoryIfMissing False [osp|wasm|]
#endif
#ifndef WASM
createWasmDirIfMissing = pure ()
#endif

main :: IO ()
main = do
  createWasmDirIfMissing
  traverse_
    ( \route' -> do
        prettiedRoute <- encodeUtf . fromMisoString $ prettyRoute route'
        let file =
              dropDrive prettiedRoute
                & if route' == minBound
                  then (</> [osp|index.html|])
                  else (<.> [osp|html|])
        putStrLn $ "prerendering: " <> show file
        IO.withFile file WriteMode $ \h -> do
          BS.hPutStr h . toHtml . modelToViews $ Model route' True
    )
    $ boundedEnumFrom minBound

outputCSSPath, indexJSPath :: MisoString
#ifdef WASM
indexJSPath = "index.js"
outputCSSPath = "../output.css"
#endif
#ifndef WASM
indexJSPath = "all.js"
outputCSSPath = "output.css"
#endif

modelToViews :: Model -> [View Model Action]
modelToViews model =
  [ doctype_,
    html_ [] $
      [ head_ [] $
          [ meta_ [charset_ "utf-8"],
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"],
            Html.title_ [] ["Fanshi1028's personal site"],
            link_ [href_ outputCSSPath, rel_ "stylesheet", type_ "text/css"]
          ],
        body_ [] $
          [ script_ [src_ indexJSPath, type_ "module", defer_ "true"] "",
            viewModel model
          ]
      ]
  ]
