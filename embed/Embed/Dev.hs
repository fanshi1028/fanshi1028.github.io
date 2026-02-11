{-# LANGUAGE TemplateHaskell #-}

module Embed.Dev where

import Data.FileEmbed

staticInputCSS = $(embedFile "static/input.css")

maplibreglJS = $(embedFile "typescript/maplibre-gl-ffi/index.js")

maplibreglCSS = $(embedFile "typescript/maplibre-gl-ffi/node_modules/maplibre-gl/dist/maplibre-gl.css")
