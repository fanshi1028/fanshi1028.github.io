{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Dashboard (dashboardComponent) where

import Control.Monad.IO.Class
import Dashboard.DataSource.HongKongObservatoryWeatherAPI
import Data.Text hiding (show)
import Haxl.Core
import Haxl.Core.Monad (flattenWT)
import Language.Javascript.JSaddle
import Miso hiding (URI)
import Miso.Html.Element
import Miso.Navigator

newtype UVIndexFetchError = UVIndexFetchError {_unUVIndexFetchError :: MisoString} deriving (Eq)

data Model
  = NoLocationData (Maybe GeolocationError)
  | NoUVIndexData (Maybe UVIndexFetchError) Geolocation
  | Model
      { _uvIndex :: UVIndex,
        _location :: Geolocation
      }
  deriving (Eq) -- TEMP FIXME

data Action
  = FetchLocationData
  | FetchUVIndexDataFromCache
  | FetchUVIndexData
  | SetLocation Geolocation
  | SetUVIndex UVIndex
  | HandleError (Either GeolocationError UVIndexFetchError) -- TEMP FIXME

makeStorageKey :: Geolocation -> MisoString
makeStorageKey (Geolocation lat long _) = ms $ show lat <> "," <> show long

defaultModel :: Model
defaultModel = NoLocationData Nothing -- TEMP FIXME

getLocation :: Model -> Either (Maybe GeolocationError) Geolocation
getLocation m = case m of
  NoLocationData mErr -> Left mErr
  NoUVIndexData _ location -> Right location
  Model _ location -> Right location

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  FetchLocationData -> geolocation SetLocation $ HandleError . Left
  FetchUVIndexDataFromCache ->
    getLocation <$> get >>= \case
      Left mErr -> io $ FetchLocationData <$ do consoleError . ms $ show mErr -- TEMP FIXME
      Right (makeStorageKey -> k) ->
        io $
          getLocalStorage k >>= \case
            Left "Not Found" -> pure $ FetchUVIndexData
            Left err ->
              FetchUVIndexData <$ do
                consoleLog (ms err)
                removeLocalStorage k -- NOTE: remove invalid cache
            Right idx -> pure $ SetUVIndex idx
  FetchUVIndexData ->
    getLocation <$> get >>= \case
      Left mErr -> io $ FetchLocationData <$ do consoleError . ms $ show mErr -- TEMP FIXME
      Right geo@(Geolocation lat long _) -> io $ do
        jscontext <- askJSM
        (r, wt) <- liftIO $ do
          -- env' <- initEnvWithData @[Text] stateEmpty jscontext undefined -- TEMP FIXME
          env' <- initEnv (stateSet HKOWeatherInformationReqState stateEmpty) jscontext
          runHaxlWithWrites
            env'
            -- { flags = defaultFlags {trace = 3}
            -- }
            $ do
              tellWrite @Text "Start Request"
              -- r1 <- dataFetch GetLocalWeaterForecast
              -- r2 <- dataFetch Get9DayWeatherForecast
              r3 <- dataFetch GetCurrentWeatherReport
              pure r3.uvindex
        traverse (consoleLog . ms) $ flattenWT wt
        consoleLog . ms $ show r
        pure $ SetUVIndex r
  SetLocation location ->
    get >>= \case
      Model _ ((== location) -> True) -> io_ $ consoleLog "same location"
      _ -> do
        put $ NoUVIndexData Nothing location
        issue FetchUVIndexData
  SetUVIndex idx ->
    getLocation <$> get >>= \case
      Left mErr -> io $ FetchLocationData <$ do consoleError . ms $ show mErr -- TEMP FIXME
      Right location -> do
        io_ $ setLocalStorage (makeStorageKey location) idx
        put $ Model idx location -- TEMP FIXME: could location be outdated?
  HandleError err -> io_ . consoleLog $ "Failed to fetch: " <> either (ms . show) _unUVIndexFetchError err

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
              Just (_unUVIndexFetchError -> err) -> case err of
                "" -> "Something went wrong while fetching the UV index for you"
                _ -> "thing went wrong while fetching the UV index for you: " <> err
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
    { initialAction = Just FetchLocationData
    }
