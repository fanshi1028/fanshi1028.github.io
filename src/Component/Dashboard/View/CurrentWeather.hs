{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Component.Dashboard.View.CurrentWeather where

import Component.Dashboard.Types
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
import Miso
import Miso.Html.Element
import Miso.Html.Event
import Miso.Html.Property hiding (label_)
import Numeric.Units.Dimensional hiding ((*), (-))
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.SIUnits hiding (toDegreeCelsiusAbsolute)
import Utils.Dimensional
import Utils.Time
import Prelude hiding (show)

stripDistrict :: Text -> Text
stripDistrict (strip -> txt) = strip . fromMaybe txt $ stripSuffix "District" txt

-- TEMP HACK FIXME: kind of fuzzy match, I am lazy to check all the district's string. I hope it works for all.
isSubstringOf :: Text -> Text -> Bool
isSubstringOf (stripDistrict -> sub) (stripDistrict -> txt) = case breakOn sub txt of
  (((== txt) -> True), "") -> False
  _ -> True

rainfallDisplay :: Bool -> Rainfall -> View model action
rainfallDisplay withPlaceLabel (Rainfall ll place _main) = case (lowerBound ll, upperBound ll) of
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

viewCurrentWeatherReport :: Bool -> Bool -> Maybe District -> Maybe UTCTime -> CurrentWeatherReport -> View Model Action
viewCurrentWeatherReport
  ifDisplayRainfall
  ifDisplayTemperature
  mFocusedDistrict
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
    div_ [class_ "flex flex-col items-start gap-2"] $
      [ div_ [class_ "group relative"] $
          [ h2_ [class_ "text-lg"] ["Today"],
            makePopover
              (Popover PlaceArrowStart PlacePopoverBottom)
              [ text . ms $ "updated " <> showRelativeTime mCurrentTime updateTime
              ]
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
                    makePopover
                      (Popover PlaceArrowStart PlacePopoverBottom)
                      [ text $ "UV index is " <> ms (show value),
                        br_ [],
                        text $ "at " <> ms place
                      ]
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
                        makePopover
                          (Popover PlaceArrowEnd PlacePopoverBottom)
                          [ text . ms $ showRelativeTime mCurrentTime recordTime,
                            br_ [],
                            text $ "at " <> place
                          ]
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
                case find (\(Temperature place@(fromMisoString -> place') _) -> place == nameEN || place' `isSubstringOf` nameEN' || nameEN' `isSubstringOf` place') _data of
                  Just (Temperature place value) ->
                    div_ [class_ "relative group"] $
                      [ text $ ms ("🌡 " <> show (toDegreeCelsiusAbsolute value)) <> " °C",
                        makePopover
                          (Popover PlaceArrowStart PlacePopoverBottom)
                          [ text . ms $ showRelativeTime mCurrentTime recordTime,
                            br_ [],
                            text $ "at " <> place
                          ]
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
        div_ [] $
          [ h3_ [class_ "sr-only"] ["Rainfall"],
            case mFocusedDistrict of
              Just (District _ nameEN@(fromMisoString -> nameEN') _) ->
                case find
                  (\(Rainfall _ place@(fromMisoString -> place') _) -> place == nameEN || place' `isSubstringOf` nameEN' || nameEN' `isSubstringOf` place')
                  _data of
                  Just i ->
                    div_ [class_ "relative group"] $
                      [ rainfallDisplay False i,
                        makePopover
                          (Popover PlaceArrowStart PlacePopoverBottom)
                          [ text $ timeIntervalDisplayText timeInterval,
                            br_ [],
                            text $ "at " <> nameEN
                          ]
                      ]
                  Nothing ->
                    div_ [] $
                      [ text $ "Error: No district matched " <> nameEN,
                        ul_ [] $ foldl' (\acc (Rainfall _ place _) -> li_ [] [text place] : acc) [] _data
                      ]
              Nothing ->
                button_ [onClick . SetDisplayRainfall $ not ifDisplayRainfall, class_ "hover:animate-wiggle border px-4 py-2"] $
                  [ p_ [] [text $ (if ifDisplayRainfall then "Hide" else "Show") <> " Rainfall"],
                    div_ [classes_ [if ifDisplayRainfall then "" else "hidden", "relative group"]] $
                      [ makePopover (Popover PlaceArrowStart PlacePopoverBottom) [text $ timeIntervalDisplayText timeInterval],
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
