{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Dashboard (dashboardComponent) where

import Control.Monad.IO.Class
import Dashboard.DataSource.BrowserGeolocationAPI
import Dashboard.DataSource.HongKongObservatoryWeatherAPI
import Dashboard.DataSource.LocalStorage
import Dashboard.DataSource.MisoRun
import Data.Function
import Data.Interval
import Data.Text hiding (foldl')
import Data.Time
import Haxl.Core
import Language.Javascript.JSaddle
import Miso hiding (URI, getLocalStorage, setLocalStorage)
import Miso.Html.Element
import Miso.Html.Event
import Miso.Html.Property hiding (label_)
import Miso.Navigator
import Numeric.Units.Dimensional hiding ((*))
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.SIUnits
import Prelude hiding (show)

haxlEnvflags :: Flags
haxlEnvflags =
  defaultFlags
#ifndef PRODUCTION
    { trace = 3,
      report = profilingReportFlags
    }
#endif

data Model
  = Model
  { _location :: Maybe (Either GeolocationError Geolocation),
    _currentWeatherReport :: Maybe CurrentWeatherReport,
    _localWeatherForecast :: Maybe LocalWeatherForecast,
    _9DayWeatherForecast :: Maybe NineDayWeatherForecast
  }
  deriving (Eq)

data Action
  = FetchWeatherData
  | SetLocation Geolocation
  | SetCurrentWeatherReport CurrentWeatherReport
  | SetLocalWeatherForecast LocalWeatherForecast
  | Set9DayWeatherForecast NineDayWeatherForecast
  deriving stock (Eq, Show)

defaultModel :: Model
defaultModel = Model Nothing Nothing Nothing Nothing

fetchDataSub :: Sink Action -> JSM ()
fetchDataSub sink = do
  jscontext <- askJSM
  liftIO $ do
    let st =
          stateEmpty
            & stateSet (MisoRunActionState jscontext sink)
            & stateSet (MisoRunJSMState jscontext)
            & stateSet (LocationReqState jscontext)
            & stateSet (HKOWeatherInformationReqState jscontext)
            & stateSet (LocalStorageReqState jscontext)

    env' <- initEnv @() st jscontext

    t <- getCurrentTime

    runHaxl (env' {flags = haxlEnvflags}) $ do
      fromLocalStorageOrDatafetch GetLocalWeatherForecast (\r -> t `diffUTCTime` r.updateTime <= 60 * 15)
        >>= misoRunAction . SetLocalWeatherForecast

      fromLocalStorageOrDatafetch Get9DayWeatherForecast (\r -> t `diffUTCTime` r.updateTime <= 60 * 60 * 12)
        >>= misoRunAction . Set9DayWeatherForecast

      uncachedRequest GetCurrentPosition >>= misoRunAction . SetLocation

      fromLocalStorageOrDatafetch GetCurrentWeatherReport (\r -> t `diffUTCTime` r.updateTime <= 60 * 15)
        >>= misoRunAction . SetCurrentWeatherReport

      fromLocalStorageOrDatafetch GetWeatherWarningSummary $ const True
      fromLocalStorageOrDatafetch GetWeatherWarningInfo $ const True
      fromLocalStorageOrDatafetch GetSpecialWeatherTips $ const True
      pure ()

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  FetchWeatherData -> startSub @Text "FetchWeatherData" fetchDataSub
  SetLocation location -> modify $ \m -> m {_location = Just (Right location)}
  SetLocalWeatherForecast w -> modify $ \m -> m {_localWeatherForecast = Just w}
  SetCurrentWeatherReport w -> modify $ \m -> m {_currentWeatherReport = Just w}
  Set9DayWeatherForecast w -> modify $ \m -> m {_9DayWeatherForecast = Just w}

viewCurrentWeatherReport :: CurrentWeatherReport -> View Model Action
viewCurrentWeatherReport wr =
  div_ [class_ "flex flex-col gap-6"] $
    [ h2_ [] ["Current Weather Report"],
      p_ [] [text . ms $ "Updated at " <> show wr.updateTime],
      div_ [class_ "flex flex-col gap-3"] $
        [ -- NOTE: skipped icon, iconUpdateTime,
          case wr.lightning of
            Nothing -> div_ [class_ "hidden"] []
            Just (DataWithInterval tt lightnings) ->
              div_ [] $
                [ h3_ [class_ "sr-only"] ["Lighting"],
                  div_ [] $
                    [ div_ [] [text . ms $ "lightning time view: " <> show tt],
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
          div_ [] $
            [ h3_ [class_ "sr-only"] ["Rainfall"],
              div_ [] $
                case wr.rainfall of
                  DataWithInterval tt rainfalls ->
                    [ div_
                        []
                        [ text . ms $ case (lowerBound tt, upperBound tt) of
                            (Finite lb, Finite ub) -> show lb <> " - " <> show ub
                            _ -> "impossible: unexpected time interval for rainfall data"
                        ],
                      ul_ [class_ "flex flex-col gap-2"] $
                        foldl'
                          ( \acc -> \case
                              Rainfall ll place _ ->
                                li_
                                  []
                                  [ div_ [class_ "flex flex-row gap-2"] $
                                      [ label_ [] [text . ms $ place <> ":"],
                                        div_
                                          []
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
                                  : acc -- NOTE: does item order matter here?
                          )
                          []
                          rainfalls
                    ]
            ],
          div_ [] $
            [ h3_ [class_ "sr-only"] ["UV Index"],
              case wr.uvindex of
                UVIndex _data desc ->
                  div_
                    []
                    [ p_ [] [text $ ms desc],
                      ul_ [] $
                        foldl'
                          ( \acc -> \(UVIndexData place value desc' mMessage) ->
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
            ],
          case foldl' (\acc msg -> li_ [] [text (ms msg)] : acc) [] wr.warningMessage of
            [] -> div_ [class_ "hidden"] []
            lis -> div_ [] $ [h3_ [class_ "sr-only"] ["Warning Message"], ul_ [class_ "flex flex-col gap-2"] lis],
          case foldl' (\acc msg -> li_ [] [text (ms msg)] : acc) [] wr.rainstormReminder of
            [] -> div_ [class_ "hidden"] []
            lis -> div_ [] $ [h3_ [class_ "sr-only"] ["Rainstorm Reminder"], ul_ [class_ "flex flex-col gap-2"] lis],
          case foldl' (\acc msg -> li_ [] [text (ms msg)] : acc) [] wr.specialWxTips of
            [] -> div_ [class_ "hidden"] []
            lis -> div_ [] $ [h3_ [class_ "sr-only"] ["Special WxTips"], ul_ [class_ "flex flex-col gap-2"] lis],
          case foldl' (\acc msg -> li_ [] [text (ms msg)] : acc) [] wr.tcmessage of
            [] -> div_ [class_ "hidden"] []
            lis -> div_ [] $ [h3_ [class_ "sr-only"] ["TC Message"], ul_ [class_ "flex flex-col gap-2"] lis],
          case wr.mintempFrom00To09 of
            Nothing -> div_ [class_ "hidden"] []
            Just "" -> div_ [class_ "hidden"] []
            Just msg -> div_ [] $ [h3_ [class_ "sr-only"] ["Min Temp From 00 To 09"], div_ [] [text $ ms msg]],
          case wr.rainfallFrom00To12 of
            Nothing -> div_ [class_ "hidden"] []
            Just "" -> div_ [class_ "hidden"] []
            Just msg -> div_ [] $ [h3_ [class_ "sr-only"] ["Rainfall From 00 To 12"], div_ [] [text $ ms msg]],
          case wr.rainfallLastMonth of
            Nothing -> div_ [class_ "hidden"] []
            Just "" -> div_ [class_ "hidden"] []
            Just msg -> div_ [] $ [h3_ [class_ "sr-only"] ["Rainfall Last Month"], div_ [] [text $ ms msg]],
          case wr.rainfallJanuaryToLastMonth of
            Nothing -> div_ [class_ "hidden"] []
            Just "" -> div_ [class_ "hidden"] []
            Just msg -> div_ [] $ [h3_ [class_ "sr-only"] ["Rainfall January To Last Month"], div_ [] [text $ ms msg]],
          div_ [] $
            [ h3_ [class_ "sr-only"] ["Temperature"],
              div_ [] $ case wr.temperature of
                DataWithRecordTime recordTime _data ->
                  [ div_ [] [text . ms $ show recordTime],
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
            ],
          div_ [] $
            [ h3_ [class_ "sr-only"] ["Humidity"],
              div_ [] $ case wr.humidity of
                DataWithRecordTime recordTime _data ->
                  [ div_ [] [text . ms $ show recordTime],
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
        ]
    ]

viewLocalWeatherForecast :: LocalWeatherForecast -> View Model Action
viewLocalWeatherForecast _ = div_ [] ["FIXME TEMP LocalWeatherForecast view not implemented"]

view9DayWeatherForecast :: NineDayWeatherForecast -> View Model Action
view9DayWeatherForecast _ = div_ [] ["FIXME TEMP 9DayWeatherForecast view not implemented"]

viewModel :: Model -> View Model Action
viewModel (Model mELocation mCurrentWeatherReport mLocalWeatherForecast m9DayWeatherForecast) =
  div_
    [class_ "flex flex-col gap-8"]
    [ case mELocation of
        Nothing -> p_ [] [text "location data loading"]
        Just (Right location) -> p_ [] [text $ "you are currently at: " <> ms (show location)]
        Just (Left (GeolocationError errCode err)) -> p_ [] [text $ "location error: " <> ms (show errCode) <> ", " <> err],
      -- case  errCode of
      --   PERMISSION_DENIED -> _
      --   POSITION_UNAVAILABLE -> _
      --   TIMEOUT -> "timeout while getting your location"
      maybe (div_ [] ["CurrentWeatherReport loading"]) viewCurrentWeatherReport mCurrentWeatherReport,
      maybe (div_ [] ["LocalWeatherForecast loading"]) viewLocalWeatherForecast mLocalWeatherForecast,
      maybe (div_ [] ["NineDayWeatherForecast loading"]) view9DayWeatherForecast m9DayWeatherForecast,
      div_
        []
        [ button_ [onClick FetchWeatherData] [text "TEMP FIXME Test: refetch"]
        ]
    ]

dashboardComponent :: Component parent Model Action
dashboardComponent = (component defaultModel updateModel viewModel) {initialAction = Just FetchWeatherData}
