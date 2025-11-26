module App.Update (updateModel) where

import App.View
import Control.Monad
import Language.Javascript.JSaddle
import Miso
import Miso.Lens
import Miso.Router
import ProductRequirementDocument

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  SetRoutingError err -> this .= RoutingError err
  GotoRoute uri -> do
    io_ . pushURI $ toURI uri
    issue $ SetURI uri
  SetURI uri -> this .= Model uri False
  SetPRDOpen setOpen -> io_ . void $ do
    prdDialgoue <- getElementById prdDialogueId
    prdDialgoue # (if setOpen then "showModal" else "close") $ ()
  AfterLoaded ->
    get >>= \case
      RoutingError err -> pure ()
      Model uri _ -> put $ Model uri False
