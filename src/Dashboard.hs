{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Dashboard (dashboardComponent) where

import Control.Monad
import Control.Monad.IO.Class
import Dashboard.DataSource.BrowserGeolocationAPI
import Dashboard.DataSource.DataGovHK.URI
import Dashboard.DataSource.HongKongObservatoryWeatherAPI
import Dashboard.DataSource.IO
import Dashboard.DataSource.JSM
import Dashboard.DataSource.LocalStorage
import Dashboard.DataSource.MisoRun
import Data.Function
import Data.Interval
import Data.Maybe
import Data.Text hiding (foldl')
import Data.Time
import Haxl.Core
import Haxl.DataSource.ConcurrentIO
import Haxl.LocalStorage
import Language.Javascript.JSaddle
import MapLibre
import Miso
import Miso.Html.Element
import Miso.Html.Event
import Miso.Html.Property hiding (label_)
import Miso.Lens
import Miso.Navigator
import Numeric.Units.Dimensional hiding ((*), (-))
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.SIUnits hiding (toDegreeCelsiusAbsolute)
import Utils.Dimensional
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
    _timeZone :: Maybe TimeZone,
    _currentWeatherReport :: Maybe CurrentWeatherReport,
    _localWeatherForecast :: Maybe LocalWeatherForecast,
    _9DayWeatherForecast :: Maybe NineDayWeatherForecast,
    _displayRainfall :: Bool,
    _displayTemperature :: Bool
  }
  deriving (Eq)

location :: Lens Model (Maybe (Either GeolocationError Geolocation))
location = lens _location $ \record x -> record {_location = x}

timeZone :: Lens Model (Maybe TimeZone)
timeZone = lens _timeZone $ \record x -> record {_timeZone = x}

currentWeatherReport :: Lens Model (Maybe CurrentWeatherReport)
currentWeatherReport = lens _currentWeatherReport $ \record x -> record {_currentWeatherReport = x}

localWeatherForecast :: Lens Model (Maybe LocalWeatherForecast)
localWeatherForecast = lens _localWeatherForecast $ \record x -> record {_localWeatherForecast = x}

nineDayWeatherForecast :: Lens Model (Maybe NineDayWeatherForecast)
nineDayWeatherForecast = lens _9DayWeatherForecast $ \record x -> record {_9DayWeatherForecast = x}

displayTemperature, displayRainfall :: Lens Model Bool
displayTemperature = lens _displayTemperature $ \record x -> record {_displayTemperature = x}
displayRainfall = lens _displayRainfall $ \record x -> record {_displayRainfall = x}

data Action
  = InitAction
  | FetchWeatherData
  | InitMapLibre
  | CleanUpMapLibre
  | SetLocation Geolocation
  | SetTimeZone TimeZone
  | SetCurrentWeatherReport CurrentWeatherReport
  | SetLocalWeatherForecast LocalWeatherForecast
  | Set9DayWeatherForecast NineDayWeatherForecast
  | SetDisplayTemperature Bool
  | SetDisplayRainfall Bool
  deriving stock (Eq, Show)

defaultModel :: Model
defaultModel = Model Nothing Nothing Nothing Nothing Nothing False False

fetchData :: Sink Action -> JSM ()
fetchData sink = do
  jscontext <- askJSM

  ioState <- liftIO mkConcurrentIOState
  let st =
        stateEmpty
          & stateSet (MisoRunActionState jscontext sink)
          & stateSet (LocationReqState jscontext)
          & stateSet (HKOWeatherInformationReqState jscontext)
          & stateSet (JSMActionState jscontext)
          & stateSet (LocalStorageReqState @HKOWeatherInformationReq jscontext)
          -- TEMP FIXME JSMActin in general should not be cached, but only when we fetch url, and that is exactly how we are abusing it.
          & stateSet (LocalStorageReqState @JSMAction jscontext)
          & stateSet ioState

  env' <- liftIO $ initEnv @() st jscontext

  _ <- liftIO . runHaxl (env' {flags = haxlEnvflags}) $ do
    tdy@(pred -> ytd) <- utctDay <$> uncachedRequest GetCurrentTime

    let getUVDataFileList = FetchURI $ appDataHKGovlistFilesURI (Just "climate-and-weather") (Just "hk-hko") Nothing (Just "uv") (periodFirstDay $ dayPeriod @Year ytd) ytd (withDefaultPaging 0)

    uncachedRequest GetCurrentTimeZone >>= misoRunAction . SetTimeZone

    fetchCacheable (GetLocalWeatherForecast tdy) >>= misoRunAction . SetLocalWeatherForecast

    fetchCacheable (Get9DayWeatherForecast tdy) >>= misoRunAction . Set9DayWeatherForecast

    fetchCacheable getUVDataFileList >>= uncachedRequest . ConsoleLog'

    uncachedRequest GetCurrentPosition >>= misoRunAction . SetLocation

    fetchCacheable (GetCurrentWeatherReport tdy) >>= misoRunAction . SetCurrentWeatherReport

    fetchCacheable $ GetWeatherWarningSummary tdy

    fetchCacheable $ GetWeatherWarningInfo tdy

    fetchCacheable $ GetSpecialWeatherTips tdy

  saveCacheToLocalStorage $ dataCache env'

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  InitAction -> issue FetchWeatherData
  InitMapLibre -> io_ $ runMapLibre createMap
  CleanUpMapLibre -> io_ cleanUpMap
  FetchWeatherData -> withSink fetchData
  SetLocation loc -> do
    io_ $ do
      mapLibreAddMarker loc
      mapLibreEaseTo loc
    location .= Just (Right loc)
  SetTimeZone tz -> timeZone .= Just tz
  SetLocalWeatherForecast w -> localWeatherForecast .= Just w
  SetCurrentWeatherReport w -> currentWeatherReport .= Just w
  Set9DayWeatherForecast w -> nineDayWeatherForecast .= Just w
  SetDisplayTemperature b -> displayTemperature .= b
  SetDisplayRainfall b -> displayRainfall .= b

viewCurrentWeatherReport :: Maybe TimeZone -> Bool -> Bool -> CurrentWeatherReport -> View Model Action
viewCurrentWeatherReport
  (fromMaybe utc -> timeZone')
  ifDisplayRainfall
  ifDisplayTemperature
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
    div_ [class_ "flex flex-col gap-6"] $
      [ div_ [class_ "flex flex-row gap-2"] $
          [ h2_ [] ["Current Weather Report"],
            p_ [] [text . ms $ "updated at " <> show (utcToLocalTime timeZone' updateTime)]
          ],
        div_ [class_ "flex flex-col gap-3"] $
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
      viewUVIndex (UVIndex [] desc) = div_ [class_ "hidden"] [text $ ms desc]
      viewUVIndex (UVIndex _data desc) =
        div_ [] $
          [ h3_ [class_ "sr-only"] ["UV Index"],
            div_ [] $
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
          ]
      viewHumidity (DataWithRecordTime recordTime _data) =
        div_ [] $
          [ h3_ [class_ "sr-only"] ["Humidity"],
            div_ [] $
              [ div_ [] [text . ms . show $ utcToLocalTime timeZone' recordTime],
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
              [ onClick . SetDisplayRainfall $ not ifDisplayTemperature,
                class_ "hover:animate-wiggle border px-4 py-2"
              ]
              $ [p_ [] [text $ (if ifDisplayTemperature then "Hide" else "Show") <> " Temperature"]],
            div_ [class_ $ if ifDisplayTemperature then "" else "hidden"] $
              [ div_ [] [text . ms . show $ utcToLocalTime timeZone' recordTime],
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
                      (Finite lb, Finite ub) -> show (utcToLocalTime timeZone' lb) <> " - " <> show (utcToLocalTime timeZone' ub)
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

viewLocalWeatherForecast :: Maybe TimeZone -> LocalWeatherForecast -> View Model Action
viewLocalWeatherForecast
  (fromMaybe utc -> timeZone')
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
           in [ p_ [] [text . ms $ "Updated at " <> show (utcToLocalTime timeZone' updateTime)],
                displayNonEmptyText generalSituation,
                displayNonEmptyText tcInfo,
                displayNonEmptyText fireDangerWarning,
                displayNonEmptyText forecastPeriod,
                displayNonEmptyText forecastDesc,
                displayNonEmptyText outlook
              ]
      ]

view9DayWeatherForecast :: Maybe TimeZone -> NineDayWeatherForecast -> View Model Action
view9DayWeatherForecast
  (fromMaybe utc -> timeZone')
  ( NineDayWeatherForecast
      weatherForecasts
      soilTemps
      seaTemp
      generalSituation -- :: StrictText,
      updateTime -- :: UTCTime
    ) =
    case foldl' (\acc weatherForecast -> viewWeatherForecast weatherForecast : acc) [] weatherForecasts of
      [] -> div_ [class_ "hidden"] []
      viewWeatherForecasts ->
        div_ [class_ "flex flex-col gap-6"] $
          [ h2_ [] [text "9 Day Weather Forecast"],
            p_ [] [text . ms $ "Updated at " <> show (utcToLocalTime timeZone' updateTime)],
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
        p_ [class_ "prose text-neutral-200"] [text . ms $ "Sea temperature is " <> show (toDegreeCelsiusAbsolute value) <> " °C in " <> place <> " at " <> show (utcToLocalTime timeZone' recordTime)]
      viewSoilTemp (SoilTemp place value recordTime depth) =
        p_ [class_ "prose text-neutral-200"] [text . ms $ "Soil temperature is " <> show (toDegreeCelsiusAbsolute value) <> " °C at " <> pack (showIn meter depth) <> " in " <> place <> " at " <> show (utcToLocalTime timeZone' recordTime)]

viewModel :: Model -> View Model Action
viewModel (Model mELocation mTimeZone mCurrentWeatherReport mLocalWeatherForecast m9DayWeatherForecast ifDisplayRainfall ifDisplayTemperature) =
  div_
    [class_ "flex flex-col gap-8 bg-neutral-600 text-neutral-200"]
    [ div_
        [ key_ @Key "mapLibreComponent",
          case mELocation of
            Just (Right _) -> class_ "h-72"
            _ -> class_ "",
          onMounted InitMapLibre,
          onBeforeUnmounted CleanUpMapLibre
        ]
        +> mapLibreComponent,
      -- case  errCode of
      --   PERMISSION_DENIED -> _
      --   POSITION_UNAVAILABLE -> _
      --   TIMEOUT -> "timeout while getting your location"
      maybe (div_ [] ["CurrentWeatherReport loading"]) (viewCurrentWeatherReport mTimeZone ifDisplayRainfall ifDisplayTemperature) mCurrentWeatherReport,
      maybe (div_ [] ["LocalWeatherForecast loading"]) (viewLocalWeatherForecast mTimeZone) mLocalWeatherForecast,
      maybe (div_ [] ["NineDayWeatherForecast loading"]) (view9DayWeatherForecast mTimeZone) m9DayWeatherForecast,
      div_ [] $ [button_ [onClick FetchWeatherData] [text "TEMP FIXME Test: refetch"]]
    ]

dashboardComponent :: Component parent Model Action
dashboardComponent = (component defaultModel updateModel viewModel) {initialAction = Just InitAction}
