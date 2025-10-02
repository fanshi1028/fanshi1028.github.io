{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Dashboard (dashboardComponent) where

import Control.Lens.Setter
import Data.Aeson
import Data.Aeson.Types
import Data.Function
import Data.List.NonEmpty
import Data.Time
import Miso hiding (URI)
import Miso.Html.Element
import Miso.Navigator
import Network.URI
import Network.URI.Lens
import Numeric.Natural

newtype UVIndexFetchError = UVIndexFetchError {_unUVIndexFetchError :: MisoString} deriving (Eq)

newtype UVIndex = UVIndex Double
  deriving stock (Eq, Show)
  deriving newtype (FromJSON)

data Model
  = NoLocationData (Maybe GeolocationError)
  | NoUVIndexData (Maybe UVIndexFetchError) Geolocation
  | Model
      { _uvIndex :: UVIndex,
        _location :: Geolocation
      }
  deriving (Eq) -- TEMP FIXME

-- uvIndex :: Lens Model Int
-- uvIndex = lens _uvIndex $ \record x -> record {_uvIndex = x}

newtype Retry = Retry Natural deriving newtype (Enum, Eq)

noRetry :: Retry
noRetry = Retry 0

data Action = FetchLocationData Retry | FetchUVIndexData Retry | SetLocation Geolocation | SetUVIndex UVIndex | HandleError (Either GeolocationError UVIndexFetchError) -- TEMP FIXME

defaultModel :: Model
defaultModel = NoLocationData Nothing -- TEMP FIXME

getLocation :: Model -> Either (Maybe GeolocationError) Geolocation
getLocation m = case m of
  NoLocationData mErr -> Left mErr
  NoUVIndexData _ location -> Right location
  Model _ location -> Right location

data UVData = UVData
  { _time :: UTCTime,
    _uvi :: UVIndex
  }

instance FromJSON UVData where
  parseJSON = withObject "UVdata" $ \o -> UVData <$> o .: "time" <*> o .: "uvi"

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  FetchLocationData (Retry _) -> geolocation SetLocation (HandleError . Left)
  FetchUVIndexData retry ->
    getLocation <$> get >>= \case
      Left mErr -> do
        io_ . consoleError . ms $ show mErr -- TEMP FIXME
        issue $ FetchLocationData retry
      Right (Geolocation lat long _) ->
        getJSON
          ( ms $
              uriToString
                id
                ( URI
                    "https:"
                    (Just $ nullURIAuth & uriRegNameLens .~ "currentuvindex.com")
                    "/api/v1/uvi"
                    ("?latitude=" <> show lat <> "&longitude=" <> show long)
                    ""
                )
                ""
          )
          []
          ( \(Response _ _ _ (v :: Value)) ->
              case iparse
                ( withObject "response" $ \o ->
                    o .: "ok" >>= \case
                      False -> fail "expected ok to be true"
                      True -> do
                        geo <- (,) <$> o .: "latitude" <*> o .: "longitude"
                        now' <- o .: "now"
                        forecast <- o .: "forecast"
                        history <- o .: "history"
                        pure (geo, history, now' :| forecast)
                )
                v of
                ISuccess (geo, _history :: [UVData], (UVData _ r) :| _) -- TEMP FIXME: ignore fastcast for now
                  | geo == (lat, long) -> SetUVIndex r
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
      Right location -> put $ Model idx location -- TEMP FIXME: could location be outdated?
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
