{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Component.Dashboard.View.WeatherForcast where

import Component.Dashboard.Types
import Component.Popover
import Data.Function
import Data.Interval
import Data.List
import Data.Maybe
import Data.Text hiding (find, foldl')
import Data.Time
import DataSource.HongKongObservatoryWeatherAPI.Types
import Miso
import Miso.Html.Element
import Miso.Html.Property hiding (label_)
import Numeric.Natural
import Numeric.Units.Dimensional hiding ((*), (-))
import Numeric.Units.Dimensional.NonSI
import Utils.Dimensional
import Utils.Time
import Prelude hiding (show)

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
