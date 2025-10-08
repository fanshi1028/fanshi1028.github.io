{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Dashboard (dashboardComponent) where

import Control.Monad.IO.Class
import Dashboard.UVIndex
import Data.Aeson
import Data.Aeson.Types
import Data.List.NonEmpty hiding (unzip)
import Data.Text hiding (show)
import Haxl.Core
import Miso hiding (URI)
import Miso.Html.Element
import Miso.Navigator
import Network.URI
import Numeric.Natural

data Model
  = NoLocationData (Maybe GeolocationError)
  | NoUVIndexData (Maybe UVIndexFetchError) Geolocation
  | Model
      { _uvIndex :: UVIndex,
        _location :: Geolocation
      }
  deriving (Eq) -- TEMP FIXME

data Action
  = FetchLocationData Retry
  | FetchUVIndexDataFromCache Retry
  | FetchUVIndexData Retry
  | SetLocation Geolocation
  | SetUVIndex UVIndex
  | HandleError (Either GeolocationError UVIndexFetchError) -- TEMP FIXME


-- TEMP FIXME: refine retry mechanism
newtype Retry = Retry Natural deriving newtype (Enum, Eq)

noRetry :: Retry
noRetry = Retry 0

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
  FetchLocationData retry ->
    geolocation
      SetLocation
      ( \err ->
          if retry == noRetry
            then HandleError $ Left err
            else FetchLocationData $ pred retry
      )
  FetchUVIndexDataFromCache retry ->
    getLocation <$> get >>= \case
      Left mErr -> do
        io_ . consoleError . ms $ show mErr -- TEMP FIXME
        issue $ FetchLocationData retry
      Right (makeStorageKey -> k) ->
        io $
          getLocalStorage k >>= \case
            Left "Not Found" -> pure $ FetchUVIndexData retry
            Left err ->
              FetchUVIndexData retry <$ do
                consoleLog (ms err)
                removeLocalStorage k -- NOTE: remove invalid cache
            Right idx -> pure $ SetUVIndex idx
  FetchUVIndexData retry ->
    getLocation <$> get >>= \case
      Left mErr -> do
        io_ . consoleError . ms $ show mErr -- TEMP FIXME
        issue $ FetchLocationData retry
      Right geo@(Geolocation lat long _) -> do
        _ <- io_ $ liftIO $ do
          env' <- initEnvWithData @[Text] stateEmpty () undefined -- TEMP FIXME
          runHaxl env' $ do
            _ <- dataFetch $ GetUVIndex undefined -- TEMP FIXME
            pure ()
        -- TEMP FIXME
        getJSON
          (ms $ uriToString id (uvIndexURI geo) "")
          []
          ( \(Response _ _ _ (v :: Value)) ->
              -- NOTE: https://currentuvindex.com/api
              case iparse
                ( withObject "response" $ \o ->
                    o .: "ok" >>= \case
                      False -> Left <$> o .: "message"
                      True -> do
                        geo' <- (,) <$> o .: "latitude" <*> o .: "longitude"
                        now' <- o .: "now"
                        forecast <- o .: "forecast"
                        history <- o .: "history"
                        pure $ Right (geo', history, now' :| forecast)
                )
                v of
                ISuccess (Left msg) -> HandleError . Right $ UVIndexFetchError msg
                ISuccess (Right (geo', _history :: [UVData], (UVData _ r) :| _forecast)) -- TEMP FIXME: ignore history & forecast for now
                  | geo' == (lat, long) -> SetUVIndex r
                  | otherwise ->
                      if retry /= noRetry
                        then FetchUVIndexData $ pred retry
                        else HandleError . Right $ UVIndexFetchError "geolocation not matched"
                IError path' err -> HandleError . Right . UVIndexFetchError . ms $ formatError path' err
          )
          (HandleError . Right . UVIndexFetchError . body)
  SetLocation location ->
    get >>= \case
      Model _ ((== location) -> True) -> io_ $ consoleLog "same location"
      _ -> do
        put $ NoUVIndexData Nothing location
        issue . FetchUVIndexData $ Retry 3
  SetUVIndex idx ->
    getLocation <$> get >>= \case
      Left mErr -> do
        io_ . consoleError . ms $ show mErr -- TEMP FIXME
        issue . FetchLocationData $ Retry 3
      Right location -> do
        io_ $ setLocalStorage (makeStorageKey location) idx
        put $ Model idx location -- TEMP FIXME: could location be outdated?
  HandleError err -> io_ . consoleLog $ "Failed to fetch: " <> either (ms . show) _unUVIndexFetchError err

-- undefined -- TEMP FIXME

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
    { initialAction = Just . FetchLocationData $ Retry 3
    }
