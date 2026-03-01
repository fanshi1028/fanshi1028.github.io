{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Component.Dashboard.View where

import Component.Dashboard.Types
import Component.Dashboard.View.CurrentWeather
import Component.Foreign.MapLibre
import Component.Popover
import Data.Function
import Data.Interval
import Data.List
import Data.List.NonEmpty
import Data.Maybe
import Data.Scientific as SCI
import Data.Text hiding (find, foldl')
import Data.Time
import DataSource.HongKongObservatoryWeatherAPI.Types
import GHC.Generics
import Miso
import Miso.Html.Element
import Miso.Html.Event
import Miso.Html.Property hiding (label_)
import Miso.Navigator
import Numeric.Natural
import Numeric.Units.Dimensional hiding ((*), (-))
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.SIUnits hiding (toDegreeCelsiusAbsolute)
import Utils.Dimensional
import Utils.JS ()
import Utils.Time
import View.SVG.LoadSpinner
import Prelude hiding (show)

viewLocalWeatherForecast :: Maybe UTCTime -> LocalWeatherForecast -> View Model Action
viewLocalWeatherForecast
  mCurrentTime
  ( LocalWeatherForecast
      generalSituation
      tcInfo
      fireDangerWarning
      _idc_forecastPeriod
      _idc_forecastDesc
      _idc_outlook
      updateTime
    ) =
    div_ [class_ "flex flex-col gap-6 group relative"] $
      let displayNonEmptyText = \case
            "" -> div_ [class_ "hidden"] []
            t -> div_ [class_ "prose"] [text $ ms t]
       in [ displayNonEmptyText generalSituation,
            displayNonEmptyText tcInfo,
            displayNonEmptyText fireDangerWarning,
            makePopover
              (Popover PlaceArrowStart PlacePopoverBottom)
              [ text . ms $
                  "Updated " <> showRelativeTime mCurrentTime updateTime
              ]
          ]

view9DayWeatherForecast :: Maybe UTCTime -> Natural -> NineDayWeatherForecast -> View Model Action
view9DayWeatherForecast
  mCurrentTime
  timeSliderValue
  ( NineDayWeatherForecast
      weatherForecasts
      _idc_about_soilTemps
      _idc_about_seaTemp
      _idc_generalSituation
      updateTime
    ) =
    div_ [] $
      [ h2_ [class_ "sr-only"] [text "Weather Forecast"],
        case weatherForecasts !? fromIntegral timeSliderValue of
          Nothing -> text . ms $ "impossible timeSliderValue: " <> show timeSliderValue
          Just
            ( WeatherForecast
                forecastDate
                weekDay
                forecastWind
                forecastWeather
                forecastTempInterval
                forecastRHInterval
                psr
                forecastIcon
              ) ->
              div_ [class_ "flex flex-col gap-2"] $
                [ div_ [class_ "group relative"] $
                    [ h2_ [class_ "text-lg"] [text . ms $ show weekDay <> " " <> show forecastDate],
                      makePopover
                        (Popover PlaceArrowStart PlacePopoverBottom)
                        [ text . ms $
                            "Updated " <> showRelativeTime mCurrentTime updateTime
                        ]
                    ],
                  div_ [] $
                    [ text . ms $ case (lowerBound forecastTempInterval, upperBound forecastTempInterval) of
                        (Finite lb, Finite ub) -> "🌡 " <> show (toDegreeCelsiusAbsolute lb) <> " - " <> show (toDegreeCelsiusAbsolute ub) <> " °C"
                        _ -> "impossible: unexpected temperature interval for forecast data"
                    ],
                  div_ [] $
                    [ text . ms $ case (lowerBound forecastRHInterval, upperBound forecastRHInterval) of
                        (Finite lb, Finite ub) -> "💧 " <> show (lb /~ percent) <> " - " <> pack (showIn percent ub)
                        _ -> "impossible: unexpected relative humidity interval for forecast data"
                    ],
                  div_ [class_ "flex flex-row gap-2 relative group"] $
                    [ "🌧",
                      input_
                        [ type_ "range",
                          min_ . ms . show . fromEnum $ minBound @ProbabilityOfSignificantRain,
                          max_ . ms . show . fromEnum $ maxBound @ProbabilityOfSignificantRain,
                          value_ . ms . show $ fromEnum psr,
                          disabled_
                        ],
                      makePopover
                        (Popover PlaceArrowStart PlacePopoverBottom)
                        [ text . ms $
                            show psr <> " probability of significant rain"
                        ]
                    ],
                  case forecastWind of
                    "" -> div_ [class_ "hidden"] []
                    _ -> div_ [] [text $ ms forecastWind],
                  case forecastWeather of
                    "" -> div_ [class_ "hidden"] ["No forecastWeather"]
                    _ -> div_ [] [text $ ms forecastWeather]
                ]
      ]

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
        ["mapLibreComponent" +> mapLibreComponent],
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
                            ( mELocation
                                >>= either
                                  (const Nothing)
                                  Just
                            )
                            mFocusedDistrict
                            mCurrentTime
                            timeSliderValue
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
