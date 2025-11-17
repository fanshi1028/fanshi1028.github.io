{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Home (home) where

import App.Types
import Data.Char
import GHC.Enum
import Miso
import Miso.Html.Element
import Miso.Html.Event
import Miso.Html.Property
import Utils.SVG.LoadSpinner

home :: Bool -> View model Action
home loading =
  div_
    [class_ "flex flex-col items-center justify-center min-h-dvh bg-neutral-200"]
    [ h1_ [class_ "sr-only"] [text $ if loading then "Tools (loading)" else "Tools"],
      ul_
        [ classes_
            [ "flex flex-col",
              "gap-4 sm:gap-5 md:gap-6 lg:gap-8 xl:gap-10 2xl:gap-12"
            ]
        ]
        $ ( \route ->
              button_
                [ onClick $ GotoRoute route,
                  classes_
                    [ if loading then "pointer-events-none animate-pulse relative" else " hover:animate-wiggle",
                      "font-bold text-neutral-700 bg-neutral-400 shadow-md shadow-neutral-600 rounded-lg",
                      "text-lg sm:text-xl md:text-2xl lg:text-3xl xl:text-4xl 2xl:text-5xl",
                      "px-4 sm:px-6 md:px-8 lg:px-10 xl:px-12 2xl:px-14"
                    ]
                ]
                [ if loading
                    then
                      loadSpinner
                        [ "size-6 sm:size-8 md:size-10 lg:size-12 xl:size-16 2xl:size-20",
                          "absolute inset-y-2.5 sm:inset-y-3 md:inset-y-3.5 lg:inset-y-5 xl:inset-y-5.5 2xl:inset-y-6"
                        ]
                    else div_ [class_ "hidden"] [],
                  div_ [classes_ ["my-2 sm:my-3 md:my-4 lg:my-6 xl:my-8 2xl:my-10"]] [text . ms $ toUpper <$> show route]
                ]
          )
          <$> boundedEnumFrom (succ minBound) -- NOTE: exclude Index
    ]
