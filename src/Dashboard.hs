{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Dashboard (dashboardComponent) where

import Control.Monad.IO.Class
import Dashboard.DataSource.BrowserGeolocationAPI
import Dashboard.DataSource.HongKongObservatoryWeatherAPI
import Dashboard.DataSource.MisoRun
import Data.Aeson
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Text
import Data.Time
import Data.Typeable
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
  deriving stock (Show, Eq)

makeReqStorageKey :: (Typeable req) => req -> MisoString
makeReqStorageKey req = ms $ showsTypeRep (typeOf req) ""

defaultModel :: Model
defaultModel = NoLocationData Nothing -- TEMP FIXME

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  FetchUVIndexData -> startSub @MisoString "FetchUVIndexData" $ \sink -> do
    jscontext <- askJSM
    let st =
          stateEmpty
            & stateSet (MisoRunActionState sink)
            & stateSet LocationReqState
            & stateSet HKOWeatherInformationReqState
        getCache :: (Request req a, FromJSON a) => req a -> (a -> Bool) -> JSM (GenHaxl u w ())
        getCache req@(makeReqStorageKey -> key) validateCache =
          getLocalStorage key >>= \case
            Left _ -> pure $ pure ()
            Right res
              | validateCache res -> pure $ cacheRequest req $ Right res
              | otherwise -> pure () <$ removeLocalStorage key
    t <- liftIO getCurrentTime
    loadCache <-
      sequence
        [ getCache GetLocalWeaterForecast $ \localWeaterForecast ->
            t `diffUTCTime` localWeaterForecast.updateTime >= 60 * 15,
          getCache Get9DayWeatherForecast $ \nineDayWeatherForecast ->
            t `diffUTCTime` nineDayWeatherForecast.updateTime >= 60 * 60 * 12,
          getCache GetCurrentWeatherReport $ \currentWeatherReport ->
            t `diffUTCTime` currentWeatherReport.updateTime >= 60 * 15,
          getCache GetWeatherWarningSummary $ const @_ @Value False, -- TEMP FIXME
          getCache GetWeatherWarningInfo $ const @_ @Value False, -- TEMP FIXME
          getCache GetSpecialWeatherTips $ const @_ @Value False -- TEMP FIXME
        ]
    -- TEMP FIXME setCache!!!!!
    (caches', wt) <- liftIO $ do
      env' <- initEnv st jscontext
      runHaxlWithWrites env' {flags = defaultFlags {trace = 3}} $ do
        sequenceA loadCache
        localWeaterForecast <- dataFetch GetLocalWeaterForecast
        dataFetch Get9DayWeatherForecast
        geo <- uncachedRequest LocationReq
        misoRunAction $ SetLocation geo
        dataFetch GetCurrentWeatherReport >>= \r ->
          misoRunAction (SetUVIndex geo r.uvindex)
        dataFetch GetWeatherWarningSummary
        dataFetch GetWeatherWarningInfo
        dataFetch GetSpecialWeatherTips
        pure $ caches env'
    traverse_ (consoleLog . ms @StrictText) $ flattenWT wt
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
dashboardComponent =
  (component defaultModel updateModel viewModel)
    { initialAction = Just FetchUVIndexData
    }
