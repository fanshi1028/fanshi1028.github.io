{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Component.Dashboard.View where

import Component.Foreign.MapLibre
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

data District = District
  { _AREA_CODE :: MisoString,
    _NAME_EN :: MisoString,
    _NAME_TC :: MisoString
  }
  deriving stock (Eq, Show, Generic)

instance FromJSVal District where
  fromJSVal v = do
    mAreaCode <- v ! "AREA_CODE" >>= fromJSVal
    mNameEN <- v ! "NAME_EN" >>= fromJSVal
    mNameTC <- v ! "NAME_TC" >>= fromJSVal
    pure $ District <$> mAreaCode <*> mNameEN <*> mNameTC

data GeoJSONDataId = FocusedDistrictBoundary | WeatherStations
  deriving stock (Eq, Show)

data Model
  = Model
  { _time :: Maybe UTCTime,
    _timeSliderValue :: Natural,
    _location :: Maybe (Either GeolocationError Geolocation),
    _focusedDistrict :: Maybe District,
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
  | FocusDistrict (Either District JSVal)
  | SetCurrentTime UTCTime
  | SetTimeSliderValue MisoString
  | SetCurrentWeatherReport CurrentWeatherReport
  | SetLocalWeatherForecast LocalWeatherForecast
  | Set9DayWeatherForecast NineDayWeatherForecast
  | AddGeoJSON GeoJSONDataId JSVal
  | SetDisplayTemperature Bool
  | SetDisplayRainfall Bool
  | ToggleDisplayHardSurfaceSoccerPitch7
  deriving stock (Eq, Show)

defaultModel :: Model
defaultModel = Model Nothing 0 Nothing Nothing Nothing Nothing Nothing False False

makePopover :: View model action -> View model action
makePopover content =
  span_
    [ classes_
        [ "invisible group-hover:visible",
          "transition-opacity opacity-0  group-hover:opacity-100",
          "absolute z-40 left-1/4 top-[120%] -mt-px",
          "bg-neutral-600 text-neutral-200 text-nowrap px-2 rounded",
          -- NOTE: arrow
          "after:border-solid after:border-8 after:border-transparent after:border-b-neutral-600",
          "after:content-[''] after:absolute after:left-2 after:bottom-full after:-mb-px"
        ]
    ]
    [content]

viewCurrentWeatherReport :: Bool -> Bool -> Maybe Geolocation -> Maybe District -> Maybe UTCTime -> Natural -> CurrentWeatherReport -> View Model Action
viewCurrentWeatherReport
  ifDisplayRainfall
  ifDisplayTemperature
  mCurrentLocation
  mFocusedDistrict
  mCurrentTime
  timeSliderValue
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
    div_ [class_ "flex flex-col gap-2"] $
      [ div_ [class_ "group relative"] $ [h2_ [class_ "text-lg"] ["Today"], makePopover . text . ms $ "updated " <> showRelativeTime mCurrentTime updateTime],
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
                              _ -> "impossible: unexpected time interval for lightning data"
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
            div_ [class_ "flex flex-row flex-wrap gap-2"] $ [viewRainfall rainfall, viewTemperature temperature, viewHumidity humidity, viewUVIndex uvindex],
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
      viewUVIndex (UVIndex (UVIndexData place value desc' mMessage :| [])) =
        div_ [] $
          [ h3_ [class_ "sr-only"] ["UV Index"],
            div_
              [class_ "flex flex-col gap-2"]
              [ div_
                  [class_ "group relative"]
                  [ div_
                      [class_ "flex flex-row gap-2"]
                      [ text "🌞",
                        input_ [type_ "range", min_ "0", max_ "11", disabled_, value_ . ms $ show value],
                        text desc'
                      ],
                    makePopover . text $ "UV Index " <> ms (show value) <> " at " <> ms place
                  ],
                div_ [] $ case mMessage of
                  Nothing -> []
                  Just message -> [text $ ms message]
              ]
          ]
      viewUVIndex (UVIndex _data) = div_ [] $ [h3_ [class_ "sr-only"] ["UV Index"], "Unexpected: FIXME more then one UV Index data"]
      viewHumidity (DataWithRecordTime recordTime _data) =
        div_ [] $
          [ h3_ [class_ "sr-only"] ["Humidity"],
            ul_ [class_ "flex flex-col gap-2"] $
              foldl'
                ( \acc (Humidity place value) ->
                    li_
                      [class_ "group relative"]
                      [ text . ms $ "💧 " <> showIn percent value,
                        makePopover . text $ "at " <> place <> " " <> ms (showRelativeTime mCurrentTime recordTime)
                      ]
                      : acc
                )
                []
                _data
          ]
      viewTemperature (DataWithRecordTime recordTime _data) =
        div_ [] $
          [ h3_ [class_ "sr-only"] ["Temperature"],
            case mFocusedDistrict of
              Just (District _ nameEN@(fromMisoString -> nameEN') _) ->
                -- TEMP HACK FIXME: kind of fuzzy match, I am lazy to check all the district's string. I hope it works for all.
                let stripDistrict (strip -> txt) = strip . fromMaybe txt $ stripSuffix "District" txt
                    isSubstringOf (stripDistrict -> sub) (stripDistrict -> txt) = case breakOn sub txt of
                      (((== txt) -> True), "") -> False
                      _ -> True
                 in case find
                      (\(Temperature place@(fromMisoString -> place') _) -> place == nameEN || place' `isSubstringOf` nameEN' || nameEN' `isSubstringOf` place')
                      _data of
                      Just i@(Temperature place value) ->
                        div_ [class_ "relative group"] $
                          [ text $ ms ("🌡 " <> show (toDegreeCelsiusAbsolute value)) <> " °C",
                            makePopover . text $ ms (showRelativeTime mCurrentTime recordTime) <> " at " <> place
                          ]
                      Nothing ->
                        div_ [] $
                          [ text $ "Error: No district matched " <> nameEN,
                            ul_ [] $ foldl' (\acc (Temperature place _) -> li_ [] [text place] : acc) [] _data
                          ]
              Nothing ->
                div_ [] $
                  [ button_
                      [ onClick . SetDisplayTemperature $ not ifDisplayTemperature,
                        class_ "hover:animate-wiggle border px-4 py-2"
                      ]
                      $ [text $ (if ifDisplayTemperature then "Hide" else "Show") <> " Temperature"],
                    div_ [class_ $ if ifDisplayTemperature then "" else "hidden"] $
                      [ text . ms $ showRelativeTime mCurrentTime recordTime,
                        ul_ [class_ "flex flex-col gap-2"] $
                          foldl' (\acc (Temperature place value) -> li_ [class_ "flex flex-row gap-2"] [text $ ms ("🌡 " <> show (toDegreeCelsiusAbsolute value)) <> " °C" <> place] : acc) [] _data
                      ]
                  ]
          ]
      timeIntervalDisplayText timeInterval = case (lowerBound timeInterval, upperBound timeInterval) of
        (Finite lb, Finite ub) -> case showInterval mCurrentTime lb ub of
          Left err -> err
          Right str -> ms str
        impossible -> "impossible! unexpected time interval for data: " <> ms (show impossible)
      viewRainfall (DataWithInterval timeInterval _data) =
        let rainfallDisplay withPlaceLabel (Rainfall ll place _main) = case (lowerBound ll, upperBound ll) of
              -- FIXME Rainfall interval better type
              (Finite a, Finite b)
                | a > b -> div_ [] ["impossible rainfall: lowerBound > upperBound"]
                | SCI.toRealFloat @Double (b /~ milli meter) == 0 ->
                    div_ [class_ "flex flex-row gap-2 min-w-fit"] $ [if withPlaceLabel then label_ [] [text . ms $ place <> ":"] else div_ [class_ "hidden"] [], "🌧 0 mm"]
                | otherwise ->
                    div_ [class_ "flex flex-row gap-2"] $
                      [ if withPlaceLabel then label_ [] [text . ms $ place <> ":"] else div_ [class_ "hidden"] [],
                        div_ [] $ [text . ms $ pack ("🌧 " <> showIn (milli meter) a <> " - " <> showIn (milli meter) b)]
                      ]
              _ -> div_ [] [text $ "impossible rainfall interval: " <> ms (show ll)]
         in div_ [] $
              [ h3_ [class_ "sr-only"] ["Rainfall"],
                case mFocusedDistrict of
                  Just (District _ nameEN@(fromMisoString -> nameEN') _) ->
                    -- TEMP FIXME: kind of fuzzy match and this is wrong, data is not recorded by district
                    let stripDistrict (strip -> txt) = strip . fromMaybe txt $ stripSuffix "District" txt
                        isSubstringOf (stripDistrict -> sub) (stripDistrict -> txt) = case breakOn sub txt of
                          (((== txt) -> True), "") -> False
                          _ -> True
                     in case find
                          (\(Rainfall _ place@(fromMisoString -> place') _) -> place == nameEN || place' `isSubstringOf` nameEN' || nameEN' `isSubstringOf` place')
                          _data of
                          Just i ->
                            div_ [class_ "relative group"] $
                              [rainfallDisplay False i, makePopover . text $ "at " <> nameEN <> " " <> timeIntervalDisplayText timeInterval]
                          Nothing ->
                            div_ [] $
                              [ text $ "Error: No district matched " <> nameEN,
                                ul_ [] $ foldl' (\acc (Rainfall _ place _) -> li_ [] [text place] : acc) [] _data
                              ]
                  Nothing ->
                    button_ [onClick . SetDisplayRainfall $ not ifDisplayRainfall, class_ "hover:animate-wiggle border px-4 py-2"] $
                      [ p_ [] [text $ (if ifDisplayRainfall then "Hide" else "Show") <> " Rainfall"],
                        div_ [classes_ [if ifDisplayRainfall then "" else "hidden", "relative group"]] $
                          [ makePopover . text $ timeIntervalDisplayText timeInterval,
                            -- NOTE: does item order matter here?
                            case foldl' (\acc i -> li_ [] [rainfallDisplay True i] : acc) [] _data of
                              [] -> div_ [] ["No Raining record"]
                              eles -> ul_ [class_ "flex flex-col gap-2"] eles
                          ]
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
    details_ [class_ "flex flex-col gap-6"] $
      [ summary_ [] ["Local Weather Forecast"],
        div_ [class_ "flex flex-col gap-4"] $
          let displayNonEmptyText = \case
                "" -> div_ [class_ "hidden"] []
                t -> div_ [class_ "prose"] [text $ ms t]
           in [ p_ [] [text . ms $ "Updated " <> showRelativeTime mCurrentTime updateTime],
                displayNonEmptyText generalSituation,
                displayNonEmptyText tcInfo,
                displayNonEmptyText fireDangerWarning,
                displayNonEmptyText forecastPeriod,
                displayNonEmptyText forecastDesc,
                displayNonEmptyText outlook
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
      generalSituation
      updateTime
    ) =
    div_ [] $
      [ h2_ [class_ "sr-only"] [text "Weather Forecast"],
        case weatherForecasts !? fromIntegral timeSliderValue of
          Nothing -> text . ms $ "impossible timeSliderValue: " <> show timeSliderValue
          Just forecast ->
            div_ [class_ "flex flex-col gap-4"] $
              [ viewWeatherForecast forecast,
                case generalSituation of
                  "" -> div_ [class_ "hidden"] []
                  _ -> div_ [class_ "prose"] [text $ ms generalSituation]
              ]
      ]
    where
      viewWeatherForecast
        ( WeatherForecast
            forecastDate
            weekDay
            forecastWind
            forecastWeather
            forecastTempInterval
            forecastRHInterval
            psr
            forecastIcon
          ) =
          div_ [class_ "flex flex-col gap-2"] $
            [ div_ [class_ "group relative"] $
                [ h2_ [class_ "text-lg"] [text . ms $ show weekDay <> " " <> show forecastDate],
                  makePopover . text . ms $ "Updated " <> showRelativeTime mCurrentTime updateTime
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
                  makePopover . text . ms $ show psr <> " probability of significant rain"
                ],
              case forecastWind of
                "" -> div_ [class_ "hidden"] []
                _ -> div_ [] [text . ms $ forecastWind],
              case forecastWeather of
                "" -> div_ [class_ "hidden"] []
                _ -> div_ [] [text . ms $ forecastWeather]
            ]

viewModel :: Model -> View Model Action
viewModel (Model mCurrentTime timeSliderValue mELocation mFocusedDistrict mCurrentWeatherReport mLocalWeatherForecast m9DayWeatherForecast rainfallDisplayMode ifDisplayTemperature) =
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
          button_
            [onClick $ ToggleDisplayHardSurfaceSoccerPitch7, class_ "group bg-neutral-200 text-neutral-600 p-2 rounded inline-block relative shadow shadow-neutral-600"]
            [ p_ [class_ "font-bold text-lg"] ["⚽"],
              makePopover "Toggle hard-surface 7-a-side football pitches"
            ],
          div_ [class_ "flex flex-col gap-2 bg-neutral-200 text-neutral-600 py-4 px-6 rounded shadow shadow-neutral-600"] $
            [ input_
                [ onInput SetTimeSliderValue,
                  type_ "range",
                  min_ "0",
                  max_ "8",
                  step_ "1",
                  value_ (ms $ show timeSliderValue)
                ],
              div_ [] $ case timeSliderValue of
                0 ->
                  [ maybe
                      ( div_
                          [class_ "flex justify-center relative group"]
                          [ loadSpinner ["size-6 sm:size-8 md:size-10 lg:size-12 xl:size-16 2xl:size-20"],
                            makePopover "CurrentWeatherReport Loading"
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
                            makePopover "LocalWeatherForecast Loading"
                          ]
                      )
                      (viewLocalWeatherForecast mCurrentTime)
                      mLocalWeatherForecast
                  ]
                _ ->
                  [ maybe
                      ( div_ [class_ "flex justify-center relative group"] $
                          [ loadSpinner ["size-6 sm:size-8 md:size-10 lg:size-12 xl:size-16 2xl:size-20"],
                            makePopover "NineDayWeatherForecast Loading"
                          ]
                      )
                      (view9DayWeatherForecast mCurrentTime timeSliderValue)
                      m9DayWeatherForecast
                  ]
            ]
        ]
    ]
