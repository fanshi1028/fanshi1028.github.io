{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Component.Dashboard.View where

import Component.Foreign.MapLibre
import Data.Function
import Data.Interval
import Data.Maybe
import Data.Text hiding (foldl')
import Data.Time
import DataSource.HongKongObservatoryWeatherAPI.Types
import Miso
import Miso.Html.Element
import Miso.Html.Event
import Miso.Html.Property hiding (label_)
import Miso.Navigator
import Numeric.Units.Dimensional hiding ((*), (-))
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.SIUnits hiding (toDegreeCelsiusAbsolute)
import Utils.Dimensional
import Utils.JS ()
import Utils.Time
import View.SVG.LoadSpinner
import Prelude hiding (show)


data GeoJSONDataId = FocusedDistrictBoundary
  deriving stock (Eq, Show)

data Model
  = Model
  { _time :: Maybe UTCTime,
    _location :: Maybe (Either GeolocationError Geolocation),
    _focusedDistrict :: Maybe StrictText, -- TEMP FIXME better type?
    _currentWeatherReport :: Maybe CurrentWeatherReport,
    _localWeatherForecast :: Maybe LocalWeatherForecast,
    _9DayWeatherForecast :: Maybe NineDayWeatherForecast,
    _displayRainfall :: Bool,
    _displayTemperature :: Bool
  }
  deriving (Eq)

data Action
  = NoOp
  | InitAction
  | FetchWeatherData
  | InitMapLibre
  | SetLocation Geolocation
  | FocusDistrict (Either StrictText JSVal)
  | SetCurrentTime UTCTime
  | SetCurrentWeatherReport CurrentWeatherReport
  | SetLocalWeatherForecast LocalWeatherForecast
  | Set9DayWeatherForecast NineDayWeatherForecast
  | AddGeoJSON GeoJSONDataId JSVal
  | SetDisplayTemperature Bool
  | SetDisplayRainfall Bool
  | ToggleDisplayHardSurfaceSoccerPitch7
  deriving stock (Eq, Show)

defaultModel :: Model
defaultModel = Model Nothing Nothing Nothing Nothing Nothing Nothing False False

viewCurrentWeatherReport :: Bool -> Bool -> Maybe UTCTime -> CurrentWeatherReport -> View Model Action
viewCurrentWeatherReport
  ifDisplayRainfall
  ifDisplayTemperature
  mCurrentTime
  ( CurrentWeatherReport
      mLightning
      rainfall
      icon
      iconUpdateTime
      uvindex
      updateTime
      warningMessage
      rainstormReminder
      specialWxTips
      tcmessage
      _mintempFrom00To09
      _rainfallFrom00To12
      _rainfallLastMonth
      _rainfallJanuaryToLastMonth
      temperature
      humidity
    ) =
    div_ [class_ "flex flex-col items-center gap-6"] $
      [ div_ [class_ "flex flex-col"] $
          [ h2_ [class_ "peer text-lg"] ["Current Weather Report"],
            p_ [class_ "peer-hover:visible invisible text-xs font-light"] [text . ms $ "updated " <> showRelativeTime mCurrentTime updateTime]
          ],
        div_ [class_ "flex flex-col md:flex-row md:flex-wrap gap-3"] $
          [ case mLightning of
              Nothing -> div_ [class_ "hidden"] []
              Just (DataWithInterval lightningsInterval lightnings) ->
                div_ [] $
                  [ h3_ [class_ "sr-only"] ["Lighting"],
                    div_ [] $
                      [ div_ [] $
                          [ text . ms $ case (lowerBound lightningsInterval, upperBound lightningsInterval) of
                              (Finite lb, Finite ub) -> show lb <> " - " <> show ub
                              _ -> "impossible: unexpected time interval for rainfall data"
                          ],
                        ul_ [] $
                          foldl'
                            ( \acc -> \case
                                Lightning place occur
                                  | occur -> li_ [] [text $ ms place] : acc -- NOTE: does item order matter here?
                                  | otherwise -> acc
                            )
                            []
                            lightnings
                      ]
                  ],
            viewRainfall rainfall,
            viewUVIndex uvindex,
            viewTemperature temperature,
            viewHumidity humidity,
            case foldl' (\acc msg -> li_ [] [text (ms msg)] : acc) [] warningMessage of
              [] -> div_ [class_ "hidden"] []
              lis -> div_ [] $ [h3_ [class_ "sr-only"] ["Warning Message"], ul_ [class_ "flex flex-col gap-2"] lis],
            case foldl' (\acc msg -> li_ [] [text (ms msg)] : acc) [] rainstormReminder of
              [] -> div_ [class_ "hidden"] []
              lis -> div_ [] $ [h3_ [class_ "sr-only"] ["Rainstorm Reminder"], ul_ [class_ "flex flex-col gap-2"] lis],
            case foldl' (\acc msg -> li_ [] [text (ms msg)] : acc) [] specialWxTips of
              [] -> div_ [class_ "hidden"] []
              lis -> div_ [] $ [h3_ [class_ "sr-only"] ["Special WxTips"], ul_ [class_ "flex flex-col gap-2"] lis],
            case foldl' (\acc msg -> li_ [] [text (ms msg)] : acc) [] tcmessage of
              [] -> div_ [class_ "hidden"] []
              lis -> div_ [] $ [h3_ [class_ "sr-only"] ["TC Message"], ul_ [class_ "flex flex-col gap-2"] lis]
          ]
      ]
    where
      viewUVIndex NoUVIndexData = div_ [class_ "hidden"] ["No uvindex data"]
      viewUVIndex (UVIndex _data) =
        div_ [] $
          [ h3_ [class_ "sr-only"] ["UV Index"],
            div_ [] $
              [ ul_ [] $
                  foldl'
                    ( \acc -> \case
                        UVIndexData place value desc' mMessage ->
                          li_
                            [class_ "flex flex-col gap-2"]
                            [ div_
                                [class_ "flex flex-row gap-2"]
                                [ label_ [] [text $ ms place],
                                  div_ [] [text . ms $ show value],
                                  div_ [] [text $ ms desc']
                                ],
                              div_ [] $ case mMessage of
                                Nothing -> []
                                Just message -> [text $ ms message]
                            ]
                            : acc
                    )
                    []
                    _data
              ]
          ]
      viewHumidity (DataWithRecordTime recordTime _data) =
        div_ [] $
          [ h3_ [class_ "sr-only"] ["Humidity"],
            div_ [] $
              [ div_ [] [text . ms $ showRelativeTime mCurrentTime recordTime],
                ul_ [class_ "flex flex-col gap-2"] $
                  foldl'
                    ( \acc (Humidity place value) ->
                        li_
                          [class_ "flex flex-row gap-2"]
                          [ label_ [] [text $ ms place <> ":"],
                            div_ [] [text . ms $ showIn percent value]
                          ]
                          : acc
                    )
                    []
                    _data
              ]
          ]
      viewTemperature (DataWithRecordTime recordTime _data) =
        div_ [] $
          [ h3_ [class_ "sr-only"] ["Temperature"],
            button_
              [ onClick . SetDisplayTemperature $ not ifDisplayTemperature,
                class_ "hover:animate-wiggle border px-4 py-2"
              ]
              $ [p_ [] [text $ (if ifDisplayTemperature then "Hide" else "Show") <> " Temperature"]],
            div_ [class_ $ if ifDisplayTemperature then "" else "hidden"] $
              [ div_ [] [text . ms $ showRelativeTime mCurrentTime recordTime],
                ul_ [class_ "flex flex-col gap-2"] $
                  foldl'
                    ( \acc (Temperature place value) ->
                        li_
                          [class_ "flex flex-row gap-2"]
                          [ label_ [] [text $ ms place <> ":"],
                            div_ [] [text . ms $ show (toDegreeCelsiusAbsolute value) <> " °C"]
                          ]
                          : acc
                    )
                    []
                    _data
              ]
          ]
      viewRainfall (DataWithInterval timeInterval _data) =
        div_ [] $
          [ h3_ [class_ "sr-only"] ["Rainfall"],
            button_
              [ onClick . SetDisplayRainfall $ not ifDisplayRainfall,
                class_ "hover:animate-wiggle border px-4 py-2"
              ]
              $ [p_ [] [text $ (if ifDisplayRainfall then "Hide" else "Show") <> " Rainfall"]],
            div_ [class_ $ if ifDisplayRainfall then "" else "hidden"] $
              [ div_ [] $
                  [ text . ms $ case (lowerBound timeInterval, upperBound timeInterval) of
                      -- TEMP FIXME
                      (Finite lb, Finite ub) -> showTime lb <> " - " <> showTime ub
                      _ -> "impossible: unexpected time interval for rainfall data"
                  ],
                ul_ [class_ "flex flex-col gap-2"] $
                  foldl'
                    ( \acc ->
                        -- NOTE: does item order matter here?
                        (: acc) . \(Rainfall ll place main) ->
                          li_ [] $
                            [ div_ [class_ "flex flex-row gap-2"] $
                                [ label_ [] [text . ms $ place <> ":"],
                                  div_ [] $
                                    [ text . ms $ case (lowerBound ll, upperBound ll) of
                                        -- FIXME Rainfall interval better type
                                        (NegInf, Finite rf) -> pack (showIn (milli meter) rf)
                                        (Finite rf1, Finite rf2) -> pack (showIn (milli meter) rf1 <> " - " <> showIn (milli meter) rf2)
                                        (_, PosInf) -> "impossible: upperBound pos inf"
                                        (_, NegInf) -> "impossible: upperBound neg inf"
                                        (PosInf, _) -> "impossible: lowerBound pos inf"
                                    ]
                                ]
                            ]
                    )
                    []
                    _data
              ]
          ]

viewLocalWeatherForecast :: Maybe UTCTime -> LocalWeatherForecast -> View Model Action
viewLocalWeatherForecast
  mCurrentTime
  ( LocalWeatherForecast
      generalSituation
      tcInfo
      fireDangerWarning
      forecastPeriod
      forecastDesc
      outlook
      updateTime
    ) =
    div_ [class_ "flex flex-col gap-6"] $
      [ h2_ [] ["Local Weather Forecast"],
        div_ [class_ "flex flex-col gap-4"] $
          let displayNonEmptyText = \case
                "" -> div_ [class_ "hidden"] []
                t -> div_ [class_ "prose text-neutral-200"] [text $ ms t]
           in [ p_ [] [text . ms $ "Updated " <> showRelativeTime mCurrentTime updateTime],
                displayNonEmptyText generalSituation,
                displayNonEmptyText tcInfo,
                displayNonEmptyText fireDangerWarning,
                displayNonEmptyText forecastPeriod,
                displayNonEmptyText forecastDesc,
                displayNonEmptyText outlook
              ]
      ]

view9DayWeatherForecast :: Maybe UTCTime -> NineDayWeatherForecast -> View Model Action
view9DayWeatherForecast
  mCurrentTime
  ( NineDayWeatherForecast
      weatherForecasts
      soilTemps
      seaTemp
      generalSituation
      updateTime
    ) =
    case foldl' (\acc weatherForecast -> viewWeatherForecast weatherForecast : acc) [] weatherForecasts of
      [] -> div_ [class_ "hidden"] []
      viewWeatherForecasts ->
        div_ [class_ "flex flex-col gap-6"] $
          [ h2_ [] [text "9 Day Weather Forecast"],
            p_ [] [text . ms $ "Updated " <> showRelativeTime mCurrentTime updateTime],
            ul_ [] viewWeatherForecasts,
            case generalSituation of
              "" -> div_ [class_ "hidden"] []
              _ -> div_ [class_ "prose text-neutral-200"] [text $ ms generalSituation],
            case foldl' (\acc soilTemp -> viewSoilTemp soilTemp : acc) [] soilTemps of
              [] -> div_ [class_ "hidden"] []
              viewSoilTemps -> ul_ [class_ "flex flex-col gap-2"] viewSoilTemps,
            viewSeaTemp seaTemp
          ]
    where
      viewWeatherForecast
        ( WeatherForecast
            forecastDate
            week
            forecastWind
            forecastWeather
            forecastTempInterval
            forecastRHInterval
            psr
            forecastIcon
          ) =
          div_ [class_ "flex flex-col gap-2"] $
            [ div_ [class_ "flex flex-row gap-2"] $
                [div_ [] [text . ms $ show week], div_ [] [text . ms $ show forecastDate]],
              case forecastWind of
                "" -> div_ [class_ "hidden"] []
                _ -> div_ [] [text . ms $ forecastWind],
              case forecastWeather of
                "" -> div_ [class_ "hidden"] []
                _ -> div_ [] [text . ms $ forecastWeather],
              div_ [] $
                [ text . ms $ case (lowerBound forecastTempInterval, upperBound forecastTempInterval) of
                    (Finite lb, Finite ub) -> show (toDegreeCelsiusAbsolute lb) <> " - " <> show (toDegreeCelsiusAbsolute ub) <> " °C"
                    _ -> "impossible: unexpected temperature interval for forecast data"
                ],
              div_ [] $
                [ text . ms $ case (lowerBound forecastRHInterval, upperBound forecastRHInterval) of
                    (Finite lb, Finite ub) -> show (lb /~ percent) <> " - " <> pack (showIn percent ub)
                    _ -> "impossible: unexpected relative humidity interval for forecast data"
                ],
              case psr of
                "" -> div_ [class_ "hidden"] []
                _ -> div_ [] [text . ms $ psr <> " probability of significant rain"]
            ]
      viewSeaTemp (SeaTemp place value recordTime) =
        p_ [class_ "prose text-neutral-200"] [text $ "Sea temperature is " <> ms (show $ toDegreeCelsiusAbsolute value) <> " °C in " <> place <> " " <> ms (showRelativeTime mCurrentTime recordTime)]
      viewSoilTemp (SoilTemp place value recordTime depth) =
        p_ [class_ "prose text-neutral-200"] [text $ "Soil temperature is " <> ms (show $ toDegreeCelsiusAbsolute value) <> " °C at " <> ms (showIn meter depth) <> " in " <> place <> " " <> ms (showRelativeTime mCurrentTime recordTime)]

viewModel :: Model -> View Model Action
viewModel (Model mCurrentTime mELocation mFocusedDistrict mCurrentWeatherReport mLocalWeatherForecast m9DayWeatherForecast ifDisplayRainfall ifDisplayTemperature) =
  div_
    [class_ "h-min-content flex flex-col gap-8 bg-neutral-600 text-neutral-200"]
    [ div_
        [ class_ $ case mELocation of
            Just (Right _) -> "h-screen w-full pb-24 -mb-24"
            _ -> "",
          onCreated InitMapLibre
        ]
        ["mapLibreComponent" +> mapLibreComponent],
      -- case  errCode of
      --   PERMISSION_DENIED -> _
      --   POSITION_UNAVAILABLE -> _
      --   TIMEOUT -> "timeout while getting your location"
      maybe
        ( div_
            [class_ "flex gap-2 justify-center"]
            [ loadSpinner ["size-6 sm:size-8 md:size-10 lg:size-12 xl:size-16 2xl:size-20"],
              "CurrentWeatherReport"
            ]
        )
        (viewCurrentWeatherReport ifDisplayRainfall ifDisplayTemperature mCurrentTime)
        mCurrentWeatherReport,
      maybe
        ( div_
            [class_ "flex gap-2 justify-center"]
            [ loadSpinner ["size-6 sm:size-8 md:size-10 lg:size-12 xl:size-16 2xl:size-20"],
              "LocalWeatherForecast"
            ]
        )
        (viewLocalWeatherForecast mCurrentTime)
        mLocalWeatherForecast,
      maybe
        ( div_
            [class_ "flex gap-2 justify-center"]
            [ loadSpinner ["size-6 sm:size-8 md:size-10 lg:size-12 xl:size-16 2xl:size-20"],
              "NineDayWeatherForecast"
            ]
        )
        (view9DayWeatherForecast mCurrentTime)
        m9DayWeatherForecast,
      div_ [class_ "z-10 absolute flex flex-col items-start gap-2 p-2"] $
        [ button_ [onClick FetchWeatherData, class_ "hidden bg-neutral-200 text-neutral-600 p-2 rounded"] [text "TEMP FIXME Test: refetch"],
          button_
            [onClick $ ToggleDisplayHardSurfaceSoccerPitch7, class_ "group bg-neutral-200 text-neutral-600 p-2 rounded inline-block relative"]
            [ "Toggle Football Pitches",
              span_
                [ classes_
                    [ "invisible group-hover:visible",
                      "transition-opacity opacity-0  group-hover:opacity-100",
                      "absolute z-20 left-1/4 top-[120%] -mt-px",
                      "bg-neutral-600 text-neutral-200 text-nowrap px-2 rounded",
                      -- NOTE: arrow
                      "after:border-solid after:border-8 after:border-transparent after:border-b-neutral-600",
                      "after:content-[''] after:absolute after:left-2 after:bottom-full after:-mb-px"
                    ]
                ]
                ["Toggle hard-surface 7-a-side football pitches"]
            ]
        ]
    ]
