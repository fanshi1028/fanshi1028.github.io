{-# LANGUAGE TemplateHaskell #-}

module Embed.CSS where

import Data.FileEmbed

staticInputCSS = $(embedFile "static/input.css")
