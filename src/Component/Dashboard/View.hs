{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Component.Dashboard.View where

import Component.Foreign.MapLibre
import Data.Function
import Data.Interval
import Data.List
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
    div_ [class_ "flex flex-col items-center gap-6"] $
      [ div_ [class_ "flex flex-col group relative"] $
          [ h2_ [class_ "text-lg"] ["Current Weather Report"],
            makePopover . text . ms $ "updated " <> showRelativeTime mCurrentTime updateTime
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
            let temperatureDisplay (Temperature place value) = ms (show $ toDegreeCelsiusAbsolute value) <> " °C at " <> place
             in case mFocusedDistrict of
                  Just (District _ nameEN@(fromMisoString -> nameEN') _) ->
                    -- TEMP HACK FIXME: kind of fuzzy match, I am lazy to check all the district's string. I hope it works for all.
                    let stripDistrict (strip -> txt) = strip . fromMaybe txt $ stripSuffix "District" txt
                        isSubstringOf (stripDistrict -> sub) (stripDistrict -> txt) = case breakOn sub txt of
                          (((== txt) -> True), "") -> False
                          _ -> True
                     in case find
                          (\(Temperature place@(fromMisoString -> place') _) -> place == nameEN || place' `isSubstringOf` nameEN' || nameEN' `isSubstringOf` place')
                          _data of
                          Just i ->
                            div_ [class_ "relative group"] $
                              [ text $ temperatureDisplay i,
                                makePopover . text $ ms (showRelativeTime mCurrentTime recordTime)
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
                              foldl' (\acc i -> li_ [class_ "flex flex-row gap-2"] [text $ temperatureDisplay i] : acc) [] _data
                          ]
                      ]
          ]
      timeIntervalDisplayText timeInterval = case (lowerBound timeInterval, upperBound timeInterval) of
        (Finite lb, Finite ub) -> case showInterval mCurrentTime lb ub of
          Left err -> err
          Right str -> ms str
        impossible -> "impossible! unexpected time interval for rainfall data: " <> ms (show impossible)
      viewRainfall (DataWithInterval timeInterval _data) =
        let rainfallDisplay (Rainfall ll place _main) = case (lowerBound ll, upperBound ll) of
              -- FIXME Rainfall interval better type
              (NegInf, _) -> Left @MisoString "impossible: lowerBound neg inf"
              (PosInf, _) -> Left "impossible: upperBound pos inf"
              (_, NegInf) -> Left "impossible: upperBound neg inf"
              (_, PosInf) -> Left "impossible: lowerBound pos inf"
              (Finite a, Finite b)
                | a > b -> Left "impossible: lowerBound > upperBound"
                | SCI.toRealFloat @Double (b /~ milli meter) == 0 -> Right "No rain"
                | otherwise ->
                    Right $
                      div_ [class_ "flex flex-row gap-2"] $
                        [ label_ [] [text . ms $ place <> ":"],
                          div_ [] $ [text . ms $ pack ("🌧 " <> showIn (milli meter) a <> " - " <> showIn (milli meter) b)]
                        ]
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
                          Just i -> case rainfallDisplay i of
                            Left err -> div_ [] [text err]
                            Right ele ->
                              div_ [class_ "relative group"] $
                                [ele, makePopover . text $ "at " <> nameEN <> " " <> timeIntervalDisplayText timeInterval]
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
                            case foldl'
                              ( \acc i -> (: acc) . (li_ []) . (: []) $ case rainfallDisplay i of
                                  Left err -> text err
                                  Right i' -> i' -- NOTE: does item order matter here?
                              )
                              []
                              _data of
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

view9DayWeatherForecast :: Maybe UTCTime -> Natural -> NineDayWeatherForecast -> View Model Action
view9DayWeatherForecast
  mCurrentTime
  timeSliderValue
  ( NineDayWeatherForecast
      weatherForecasts
      soilTemps
      seaTemp
      generalSituation
      updateTime
    )
    | timeSliderValue == 0 = div_ [class_ "hidden"] []
    | otherwise =
        case weatherForecasts !? fromIntegral timeSliderValue of
          Nothing -> div_ [] [text . ms $ "impossible timeSliderValue: " <> show timeSliderValue]
          Just forecast ->
            div_ [class_ "flex flex-col gap-6"] $
              [ h2_ [class_ "sr-only"] [text "9 Day Weather Forecast"],
                p_ [] [text . ms $ "Updated " <> showRelativeTime mCurrentTime updateTime],
                -- ul_ [] viewWeatherForecasts,
                viewWeatherForecast forecast,
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
viewModel (Model mCurrentTime timeSliderValue mELocation mFocusedDistrict mCurrentWeatherReport mLocalWeatherForecast m9DayWeatherForecast rainfallDisplayMode ifDisplayTemperature) =
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
      case timeSliderValue of
        0 ->
          div_ [] $
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
          maybe
            ( div_ [class_ "flex justify-center relative group"] $
                [ loadSpinner ["size-6 sm:size-8 md:size-10 lg:size-12 xl:size-16 2xl:size-20"],
                  makePopover "NineDayWeatherForecast Loading"
                ]
            )
            (view9DayWeatherForecast mCurrentTime timeSliderValue)
            m9DayWeatherForecast,
      div_ [class_ "z-10 absolute flex flex-col items-start gap-2 p-2"] $
        [ input_
            [ onInput SetTimeSliderValue,
              type_ "range",
              min_ "0",
              max_ "8",
              step_ "1",
              value_ (ms $ show timeSliderValue)
            ],
          button_ [onClick FetchWeatherData, class_ "hidden bg-neutral-200 text-neutral-600 p-2 rounded"] [text "TEMP FIXME Test: refetch"],
          button_
            [onClick $ ToggleDisplayHardSurfaceSoccerPitch7, class_ "group bg-neutral-200 text-neutral-600 p-2 rounded inline-block relative"]
            [ p_ [class_ "font-bold text-lg"] ["⚽"],
              makePopover "Toggle hard-surface 7-a-side football pitches"
            ]
        ]
    ]
