{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#ifdef WASM
{-# LANGUAGE QuasiQuotes #-}
#endif

module Main where

import App
import Data.ByteString.Lazy as BS
import Data.Char
import Data.Foldable
import GHC.Enum
import Miso
import Miso.Html.Element as Html
import Miso.Html.Property
import Miso.Html.Render
import System.File.OsPath as IO
import System.OsPath
import UnliftIO

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
    ( \route -> do
        file <- (<.>) <$> encodeUtf (toLower <$> show route) <*> encodeUtf ".html"
        withRunInIO $ \runInIO -> IO.withFile file WriteMode $ \h ->
          runInIO . BS.hPutStr h . toHtml . modelToViews $ Model route True
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
