{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Home (home) where

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
      ul_ [] [button_ [onClick $ GotoRoute Pomodoro, class_ "font-bold text-lg sm:text-xl md:text-2xl lg:text-3xl xl:text-4xl px-4 sm:px-6 md:px-8 lg:px-10 xl:px-12 py-2 sm:py-3 md:py-4 lg:py-6 xl:py-8 text-neutral-700 bg-neutral-400 shadow-md shadow-neutral-600 rounded-lg hover:animate-wiggle"] ["Pomodoro"]]
    ]
