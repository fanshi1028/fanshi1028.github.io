module Utils.Fetch.CSV where

import Control.Exception
import Data.ByteString.Lazy (fromStrict)
import Data.Csv
import Data.Functor
import Data.Text
import Data.Text.Encoding
import Data.Vector
import Haxl.Core.Exception
import Language.Javascript.JSaddle.Types
import Network.URI
import Utils.Fetch

fetchGetCSV :: (FromRecord a) => URI -> JSM (Either SomeException (Vector a))
fetchGetCSV uri = do
  fetchGetText uri <&> \case
    Left err -> Left err
    Right txt -> case decode HasHeader . fromStrict $ encodeUtf8 txt of
      Left err -> Left . toException . UnexpectedType $ pack err
      Right r -> Right r
