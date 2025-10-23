{-# LANGUAGE ApplicativeDo #-}
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
import Data.Functor
import Data.Text
import Data.Time
import Haxl.Core
import Language.Javascript.JSaddle
import Miso hiding (URI, getLocalStorage, setLocalStorage)
import Miso.Html.Element
import Miso.Html.Event
import Miso.Navigator
import Prelude hiding (show)

data Model
  = NoLocationData (Maybe GeolocationError)
  | NoUVIndexData
      (Maybe StrictText) -- NOTE TEMP: HaxlException as Text because it has no Eq instance
      Geolocation
  | Model
      { _uvIndex :: UVIndex,
        _location :: Geolocation
      }
  deriving (Eq) -- TEMP FIXME

data Action
  = FetchUVIndexData
  | SetLocation Geolocation
  | SetUVIndex Geolocation UVIndex
  deriving stock (Show, Eq)

defaultModel :: Model
defaultModel = NoLocationData Nothing -- TEMP FIXME

fetchDataSub :: Sink Action -> JSM ()
fetchDataSub sink = do
  jscontext <- askJSM
  liftIO $ do
    let st =
          stateEmpty
            & stateSet (MisoRunActionState sink)
            & stateSet LocationReqState
            & stateSet HKOWeatherInformationReqState
            & stateSet LocalStorageReqState

    env' <- initEnv @() st jscontext

    t <- getCurrentTime

    runHaxl
      env'
        { flags =
            defaultFlags
              { trace = 3
              -- report = profilingReportFlags
              }
        }
      $ () <$ do
        fromLocalStorageOrDatafetch GetLocalWeaterForecast (\r -> t `diffUTCTime` r.updateTime <= 60 * 15)
        fromLocalStorageOrDatafetch Get9DayWeatherForecast (\r -> t `diffUTCTime` r.updateTime <= 60 * 60 * 12)

        geo <- uncachedRequest LocationReq
        misoRunAction $ SetLocation geo

        currentWeatherReport <- fromLocalStorageOrDatafetch GetCurrentWeatherReport (\r -> t `diffUTCTime` r.updateTime <= 60 * 15)

        misoRunAction $ SetUVIndex geo currentWeatherReport.uvindex

        -- TEMP FIXME
        fromLocalStorageOrDatafetch GetWeatherWarningSummary $ const True
        fromLocalStorageOrDatafetch GetWeatherWarningInfo $ const True
        fromLocalStorageOrDatafetch GetSpecialWeatherTips $ const True

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  FetchUVIndexData -> startSub @Text "FetchUVIndexData" fetchDataSub
  SetLocation location ->
    get >>= \case
      Model _ ((== location) -> True) -> io_ $ consoleLog "same location"
      _ -> do
        put $ NoUVIndexData Nothing location
        issue FetchUVIndexData
  SetUVIndex geo idx -> put $ Model idx geo -- TEMP FIXME: could location be outdated?

-- TEMP FIXME
viewModel :: Model -> View Model Action
viewModel = \case
  NoLocationData mErr -> case mErr of
    Nothing -> div_ [] [text "No location data: need to know where you are at to tell you the uv index"]
    Just (GeolocationError errCode err) ->
      div_
        []
        [ p_ [] [text "No location data: need to know where you are at to tell you the uv index"],
          br_ [],
          p_ [] [text $ "location error: " <> ms (show errCode) <> ", " <> err]
          -- case  errCode of
          --   PERMISSION_DENIED -> _
          --   POSITION_UNAVAILABLE -> _
          --   TIMEOUT -> "timeout while getting your location"
        ]
  NoUVIndexData mErr location ->
    div_ [] $
      [ text $ "you are currently at: " <> ms (show location),
        br_ [],
        text $
          "We are sorry. "
            <> case mErr of
              Nothing -> "We have no UV index data for you"
              Just err -> case err of
                "" -> "Something went wrong while fetching the UV index for you"
                _ -> "thing went wrong while fetching the UV index for you: " <> ms err
      ]
  Model uvIndex location ->
    div_ [] $
      [ text $ "you are currently at: " <> ms (show location),
        br_ [],
        text $ "current uv index is: " <> ms (show uvIndex)
      ]

dashboardComponent :: Component parent Model Action
dashboardComponent = (component defaultModel updateModel viewModel) {initialAction = Just FetchUVIndexData}
