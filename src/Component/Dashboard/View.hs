{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Component.Dashboard.View where

import Component.Dashboard.Types
import Component.Dashboard.View.CurrentWeather
import Component.Dashboard.View.WeatherForcast
import Component.Foreign.MapLibre
import Component.Popover
import Data.Text hiding (find, foldl')
import Miso
import Miso.Html.Element
import Miso.Html.Event
import Miso.Html.Property hiding (label_)
import Utils.JS ()
import View.SVG.LoadSpinner
import Prelude hiding (show)

viewModel :: Model -> View Model Action
viewModel (Model mCurrentTime timeSliderValue mELocation mFocusedDistrict mCurrentWeatherReport mLocalWeatherForecast m9DayWeatherForecast ifDisplayWeatherPanel rainfallDisplayMode ifDisplayTemperature) =
  div_
    [class_ "h-min-content flex flex-col gap-8 bg-neutral-600 text-neutral-200"]
    [ div_
        [ class_ $ case mELocation of
            Just (Right _) -> "h-screen w-full"
            _ -> "",
          onCreated InitMapLibre
        ]
        ["mapLibreComponent" +> Component.Foreign.MapLibre.mapLibreComponent],
      -- case  errCode of
      --   PERMISSION_DENIED -> _
      --   POSITION_UNAVAILABLE -> _
      --   TIMEOUT -> "timeout while getting your location"
      div_ [class_ "z-10 absolute flex flex-col items-start gap-2 p-2 max-w-xs"] $
        [ button_ [onClick FetchWeatherData, class_ "hidden bg-neutral-200 text-neutral-600 p-2 rounded"] [text "TEMP FIXME Test: refetch"],
          div_
            [class_ "flex flex-row gap-2"]
            [ button_
                [onClick ToggleDisplayHardSurfaceSoccerPitch7, class_ "group bg-neutral-200 text-neutral-600 p-2 rounded inline-block relative shadow shadow-neutral-600"]
                [ p_ [class_ "font-bold text-lg"] ["⚽"],
                  makePopover (Popover PlaceArrowStart PlacePopoverBottom) ["Toggle hard-surface 7-a-side football pitches"]
                ],
              button_
                [ onClick ToggleDisplayWeatherPanel,
                  class_ "group bg-neutral-200 text-neutral-600 p-2 rounded inline-block relative shadow shadow-neutral-600"
                ]
                [ p_ [class_ "font-bold text-lg"] ["🌞"],
                  makePopover (Popover PlaceArrowStart PlacePopoverBottom) ["Toggle weather info panel"]
                ]
            ],
          div_
            [ classes_
                [ "gap-2 bg-neutral-200 text-neutral-600 py-4 px-6 rounded shadow shadow-neutral-600",
                  if ifDisplayWeatherPanel then "flex flex-col" else "hidden"
                ]
            ]
            $ [ input_
                  [ onInput SetTimeSliderValue,
                    type_ "range",
                    min_ "0",
                    max_ "8",
                    step_ "1",
                    value_ (ms $ show timeSliderValue)
                  ],
                div_ [class_ "flex flex-col gap-4"] $ case timeSliderValue of
                  0 ->
                    [ maybe
                        ( div_
                            [class_ "flex justify-center relative group"]
                            [ loadSpinner ["size-6 sm:size-8 md:size-10 lg:size-12 xl:size-16 2xl:size-20"],
                              makePopover (Popover PlaceArrowStart PlacePopoverBottom) $ ["CurrentWeatherReport Loading"]
                            ]
                        )
                        ( viewCurrentWeatherReport
                            rainfallDisplayMode
                            ifDisplayTemperature
                            mFocusedDistrict
                            mCurrentTime
                        )
                        mCurrentWeatherReport,
                      maybe
                        ( div_
                            [class_ "flex justify-center relative group"]
                            [ loadSpinner ["size-6 sm:size-8 md:size-10 lg:size-12 xl:size-16 2xl:size-20"],
                              makePopover (Popover PlaceArrowStart PlacePopoverBottom) $ ["LocalWeatherForecast Loading"]
                            ]
                        )
                        (viewLocalWeatherForecast mCurrentTime)
                        mLocalWeatherForecast
                    ]
                  _ ->
                    [ maybe
                        ( div_ [class_ "flex justify-center relative group"] $
                            [ loadSpinner ["size-6 sm:size-8 md:size-10 lg:size-12 xl:size-16 2xl:size-20"],
                              makePopover (Popover PlaceArrowStart PlacePopoverBottom) ["NineDayWeatherForecast Loading"]
                            ]
                        )
                        (view9DayWeatherForecast mCurrentTime $ timeSliderValue - 1)
                        m9DayWeatherForecast
                    ]
              ]
        ]
    ]
