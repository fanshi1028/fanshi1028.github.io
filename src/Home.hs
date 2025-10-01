{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Home (home) where

import Data.Char
import GHC.Enum
import Miso
import Miso.Html.Element
import Miso.Html.Event
import Miso.Html.Property
import Route

home :: View model Action
home =
  div_
    [class_ "flex flex-col items-center justify-center min-h-dvh bg-neutral-200"]
    [ h1_ [class_ "sr-only"] [text "Tools"],
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
                    [ "font-bold text-neutral-700 bg-neutral-400 shadow-md shadow-neutral-600 rounded-lg hover:animate-wiggle",
                      "text-lg sm:text-xl md:text-2xl lg:text-3xl xl:text-4xl 2xl:text-5xl",
                      "px-4 sm:px-6 md:px-8 lg:px-10 xl:px-12 2xl:px-14",
                      "py-2 sm:py-3 md:py-4 lg:py-6 xl:py-8 2xl:py-10"
                    ]
                ]
                [text . ms $ toUpper <$> show route]
          )
          <$> boundedEnumFrom (succ minBound) -- NOTE: exclude Index
    ]
