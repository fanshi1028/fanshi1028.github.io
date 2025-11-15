{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.ByteString.Lazy as BS
import Data.Char
import Data.Foldable
import GHC.Enum
import Miso
import Miso.Html.Element as Html
import Miso.Html.Property
import Miso.Html.Render
import Route
import Route.View
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
          runInIO . BS.hPutStr h . toHtml . wrapHtml False . navView $ Model route

        withRunInIO $ \runInIO -> IO.withFile (wasmDir </> file) WriteMode $ \h ->
          runInIO . BS.hPutStr h . toHtml . wrapHtml True . navView $ Model route
    )
    $ boundedEnumFrom minBound

wrapHtml :: Bool -> View model action -> [View model action]
wrapHtml useWasm vw =
  [ doctype_,
    html_ [] $
      [ head_ [] $
          [ meta_ [charset_ "utf-8"],
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"],
            Html.title_ [] ["Fanshi1028's personal site"],
            link_ [href_ "output.css", rel_ "stylesheet", type_ "text/css"]
          ],
        body_ [] $
          [ script_ [src_ $ if useWasm then "index.js" else "all.js", type_ "module", defer_ "true"] "",
            vw
          ]
      ]
  ]
