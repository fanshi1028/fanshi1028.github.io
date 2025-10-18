{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Dashboard (dashboardComponent) where

import Control.Monad.IO.Class
import Dashboard.DataSource.BrowserGeolocationAPI
import Dashboard.DataSource.HongKongObservatoryWeatherAPI
import Data.Text
import Haxl.Core
import Haxl.Core.Monad (flattenWT)
import Language.Javascript.JSaddle
import Miso hiding (URI)
import Miso.Html.Element
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

makeStorageKey :: Geolocation -> MisoString
makeStorageKey (Geolocation lat long _) = ms $ show lat <> "," <> show long

defaultModel :: Model
defaultModel = NoLocationData Nothing -- TEMP FIXME

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  -- Right (makeStorageKey -> k) ->
  --   io $
  --     getLocalStorage k >>= \case
  --       Left "Not Found" -> pure $ FetchUVIndexData
  --       Left err ->
  --         FetchUVIndexData <$ do
  --           consoleLog (ms err)
  --           removeLocalStorage k -- NOTE: remove invalid cache
  --       Right idx -> pure $ SetUVIndex idx
  FetchUVIndexData -> for $ do
    jscontext <- askJSM
    ((geo, r3), wt) <- liftIO $ do
      env' <- initEnv (stateSet LocationReqState $ stateSet HKOWeatherInformationReqState stateEmpty) jscontext
      runHaxlWithWrites env'
       -- {flags = defaultFlags {trace = 3}}
       $ do
          r1 <- dataFetch GetLocalWeaterForecast
          r2 <- dataFetch Get9DayWeatherForecast
          r3 <- dataFetch GetCurrentWeatherReport
          r4 <- dataFetch GetWeatherWarningSummary
          r5 <- dataFetch GetWeatherWarningInfo
          r5 <- dataFetch GetSpecialWeatherTips
          geo <- dataFetch LocationReq
          pure (geo, r3)
    traverse (consoleLog . ms @StrictText) $ flattenWT wt
    pure $ [SetLocation geo, SetUVIndex geo r3.uvindex]
  SetLocation location ->
    get >>= \case
      Model _ ((== location) -> True) -> io_ $ consoleLog "same location"
      _ -> do
        put $ NoUVIndexData Nothing location
        issue FetchUVIndexData
  SetUVIndex geo idx -> do
    io_ $ setLocalStorage (makeStorageKey geo) idx
    put $ Model idx geo -- TEMP FIXME: could location be outdated?

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
dashboardComponent =
  (component defaultModel updateModel viewModel)
    { initialAction = Just FetchUVIndexData
    }
