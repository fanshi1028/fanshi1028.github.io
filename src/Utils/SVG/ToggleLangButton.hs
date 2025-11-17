{-# LANGUAGE OverloadedStrings #-}

module Utils.SVG.ToggleLangButton (toJsButtonSVG, toWasmButtonSVG) where

import Miso
import Miso.Html
import Miso.Html.Property
import Miso.Svg.Element
import Miso.Svg.Property hiding (path_)

jsLogoSVGPath, wasmLogoSVGPath :: View model action
jsLogoSVGPath =
  path_
    [ class_ "fill-[#F7DF1E]",
      d_ "M0 0h24v24H0V0zm22.034 18.276c-.175-1.095-.888-2.015-3.003-2.873-.736-.345-1.554-.585-1.797-1.14-.091-.33-.105-.51-.046-.705.15-.646.915-.84 1.515-.66.39.12.75.42.976.9 1.034-.676 1.034-.676 1.755-1.125-.27-.42-.404-.601-.586-.78-.63-.705-1.469-1.065-2.834-1.034l-.705.089c-.676.165-1.32.525-1.71 1.005-1.14 1.291-.811 3.541.569 4.471 1.365 1.02 3.361 1.244 3.616 2.205.24 1.17-.87 1.545-1.966 1.41-.811-.18-1.26-.586-1.755-1.336l-1.83 1.051c.21.48.45.689.81 1.109 1.74 1.756 6.09 1.666 6.871-1.004.029-.09.24-.705.074-1.65l.046.067zm-8.983-7.245h-2.248c0 1.938-.009 3.864-.009 5.805 0 1.232.063 2.363-.138 2.711-.33.689-1.18.601-1.566.48-.396-.196-.597-.466-.83-.855-.063-.105-.11-.196-.127-.196l-1.825 1.125c.305.63.75 1.172 1.324 1.517.855.51 2.004.675 3.207.405.783-.226 1.458-.691 1.811-1.411.51-.93.402-2.07.397-3.346.012-2.054 0-4.109 0-6.179l.004-.056z"
    ]
wasmLogoSVGPath =
  path_
    [ class_ "fill-[#654FF0]",
      d_ "M14.745,0c0,0.042,0,0.085,0,0.129c0,1.52-1.232,2.752-2.752,2.752c-1.52,0-2.752-1.232-2.752-2.752 c0-0.045,0-0.087,0-0.129H0v24h24V0H14.745z M11.454,21.431l-1.169-5.783h-0.02l-1.264,5.783H7.39l-1.824-8.497h1.59l1.088,5.783 h0.02l1.311-5.783h1.487l1.177,5.854h0.02l1.242-5.854h1.561l-2.027,8.497H11.454z M20.209,21.431l-0.542-1.891h-2.861l-0.417,1.891 h-1.59l2.056-8.497h2.509l2.5,8.497H20.209z M17.812,15.028l-0.694,3.118h2.159l-0.796-3.118H17.812z"
    ]

toWasmButtonSVG :: View model action
toWasmButtonSVG =
  svg_
    [ classes_
        [ "bg-neutral-300",
          "size-6 sm:size-8 md:size-10 lg:size-12 xl:size-16 2xl:size-20",
          "hover:size-8 sm:hover:size-10 md:hover:size-12 lg:hover:size-16 xl:hover:size-20 2xl:hover:size-24"
        ],
      xmlns_ "http://www.w3.org/2000/svg",
      viewBox_ "-12 0 36 24"
    ]
    [ wasmLogoSVGPath,
      path_
        [ classes_ ["stroke-2 stroke-purple-400"],
          transform_ "translate(-12,0)",
          strokeLinecap_ "round",
          strokeLinejoin_ "round",
          d_ "M7.5 21 3 16.5m0 0L7.5 12M3 16.5h13.5m0-13.5L21 7.5m0 0L16.5 12M21 7.5H7.5"
        ]
    ]

toJsButtonSVG :: View model action
toJsButtonSVG =
  svg_
    [ classes_
        [ "bg-neutral-600",
          "size-6 sm:size-8 md:size-10 lg:size-12 xl:size-16 2xl:size-20",
          "hover:size-8 sm:hover:size-10 md:hover:size-12 lg:hover:size-16 xl:hover:size-20 2xl:hover:size-24"
        ],
      xmlns_ "http://www.w3.org/2000/svg",
      viewBox_ "-12 0 36 24"
    ]
    [ jsLogoSVGPath,
      path_
        [ classes_ ["stroke-2 stroke-orange-600"],
          transform_ "translate(-12,0)",
          strokeLinecap_ "round",
          strokeLinejoin_ "round",
          d_ "M7.5 21 3 16.5m0 0L7.5 12M3 16.5h13.5m0-13.5L21 7.5m0 0L16.5 12M21 7.5H7.5"
        ]
    ]
