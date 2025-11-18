{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

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
import System.Directory.OsPath
import System.File.OsPath as IO
import System.OsPath
import UnliftIO

main :: IO ()
main = do
  let wasmDir = [osp|wasm|]
  createDirectoryIfMissing False wasmDir
  traverse_
    ( \route -> do
        file <- (<.>) <$> encodeUtf (toLower <$> show route) <*> encodeUtf ".html"
        withRunInIO $ \runInIO -> IO.withFile file WriteMode $ \h ->
          runInIO . BS.hPutStr h . toHtml . modelToViews $ Model route True
    )
    $ boundedEnumFrom minBound

outputCSSPath :: MisoString
#ifdef wasm32_HOST_ARCH
outputCSSPath = "../output.css"
#endif
#ifndef wasm32_HOST_ARCH
outputCSSPath = "output.css"
#endif

indexJSPath :: MisoString
#ifdef wasm32_HOST_ARCH
indexJSPath = "index.js"
#endif
#ifndef wasm32_HOST_ARCH
indexJSPath = "all.js"
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
