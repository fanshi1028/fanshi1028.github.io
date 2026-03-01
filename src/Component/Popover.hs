{-# LANGUAGE OverloadedStrings #-}

module Component.Popover where

import Miso
import Miso.Html.Element
import Miso.Html.Property hiding (label_)
import Utils.JS ()
import Prelude hiding (show)

data ArrowPlacement = PlaceArrowStart | PlaceArrowEnd

data PopoverPlacement = PlacePopoverLeft | PlacePopoverTop | PlacePopoverRight | PlacePopoverBottom

data Popover = Popover ArrowPlacement PopoverPlacement

makePopover :: Popover -> [View model action] -> View model action
makePopover config content =
  span_
    [ classes_
        [ "invisible group-hover:visible",
          "transition-opacity opacity-0 group-hover:opacity-100",
          "absolute z-40",
          "w-max max-w-72 text-pretty",
          "text-left",
          "bg-neutral-600 text-neutral-200 px-2 rounded",
          "after:border-solid after:border-8 after:border-transparent",
          "after:content-[''] after:absolute",
          case config of
            Popover PlaceArrowStart _ -> "left-2 after:left-2"
            Popover PlaceArrowEnd _ -> "right-2 after:right-2",
          case config of
            Popover _ PlacePopoverBottom ->
              "top-[120%] -mt-px after:bottom-full after:-mb-px after:border-b-neutral-600"
            Popover _ PlacePopoverTop ->
              "bottom-[120%] -mb-px after:top-full after:-mt-px after:border-t-neutral-600"
            Popover _ PlacePopoverLeft ->
              "right-[120%] -mr-px after:left-full after:-ml-px after:border-l-neutral-600"
            Popover _ PlacePopoverRight ->
              "left-[120%] -ml-px after:right-full after:-mr-px after:border-r-neutral-600"
        ]
    ]
    content
