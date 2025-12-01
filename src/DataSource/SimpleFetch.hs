{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module DataSource.SimpleFetch where

import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding
import Control.Exception (SomeException)
import Data.Csv hiding (decode, encode)
import Data.Hashable
import Data.Text hiding (concat, elem, foldl', foldr, reverse, show)
import Data.Text qualified as T
import Data.Typeable
import Data.Vector (Vector)
import DataSource.LocalStorage
import Haxl.Core
import Language.Javascript.JSaddle
import Network.URI
import Network.URI.Static
import Numeric.Units.Dimensional
import Numeric.Units.Dimensional.Quantities
import Numeric.Units.Dimensional.SIUnits
import Text.Read
import Utils.Haxl
import Prelude hiding ((+))

data SimpleFetch a where
  FetchJSON :: forall a. (FromJSVal a) => URI -> SimpleFetch a
  FetchText :: URI -> SimpleFetch StrictText
  FetchCSV :: forall a. (FromRecord a) => Bool -> URI -> SimpleFetch (Vector a)

deriving instance Eq (SimpleFetch a)

deriving instance Show (SimpleFetch a)

instance ShowP SimpleFetch where showp = show

instance StateKey SimpleFetch where
  data State SimpleFetch = JSMActionState JSContextRef

instance Hashable (SimpleFetch a) where
  hashWithSalt s =
    hashWithSalt @Int s . \case
      FetchJSON uri' -> s `hashWithSalt` (0 :: Int) `hashWithSalt` uriToString id uri' ""
      FetchText uri' -> s `hashWithSalt` (1 :: Int) `hashWithSalt` uriToString id uri' ""
      FetchCSV hasHeader uri' ->
        s
          `hashWithSalt` (2 :: Int)
          `hashWithSalt` hasHeader
          `hashWithSalt` uriToString id uri' ""

instance DataSourceName SimpleFetch where
  dataSourceName _ = T.show . typeRepTyCon . typeRep $ Proxy @SimpleFetch

instance DataSource u SimpleFetch where
  fetch _state@(JSMActionState jsContext) = backgroundFetchPar (runJSaddle jsContext . performJSM) _state
    where
      performJSM :: SimpleFetch a -> JSM (Either SomeException a)
      performJSM = \case
        FetchJSON uri' -> fetchGetJSON Proxy uri'
        FetchText uri' -> fetchGetText uri'
        FetchCSV hasHeader uri' -> fetchGetCSV Proxy (if hasHeader then HasHeader else NoHeader) uri'

-- NOTE: https://www.lcsd.gov.hk/datagovhk/facility/facility-hssp7_data_dictionary.pdf
{- example item <2025-12-01 Mon>:
{
    "Address_cn": "九龍何文田忠義街一號",
    "Address_en": "No.1 Chung Yee Street, Ho Man Tin, Kowloon.",
    "Ancillary_facilities_cn": "<ul>\r\n<li>男、女更衣室及洗手間</li>\r\n<li>1個收費停車場 (設有1個殘疾人士專用車位)</li>\r\n<li>其他設施包括2個籃球場、1個兒童遊樂場、1條緩跑徑及6個健身站</li>\r\n<li>此球場可進行7人足球或手球活動</li>\r\n<li>設有可容納480人的看台</li>\r\n<li>無障礙設施：暢通易達洗手間、觸覺引路帶、觸覺點字及觸覺平面圖</li>\r\n</ul>",
    "Ancillary_facilities_en": "<ul>\r\n<li>Men's and ladies' changing rooms and toilets</li>\r\n<li>A fee-charging car park (including 1 designated disabled parking space)</li>\r\n<li>Other facilities include 2 basketball courts, a children's playground and a jogging track with 6 fitness stations.</li>\r\n<li>This court can be used for playing 7-a-side soccer or handball.</li>\r\n<li>A spectator stand with 480 seats is provided.</li>\r\n<li>Barrier Free Facilities: Accessible Toilet, Tactile Guide Path, Braille Directory Map/Floor Plan</li>\r\n</ul>",
    "Court_no_cn": "1",
    "Court_no_en": "1",
    "District_cn": "九龍城區",
    "District_en": "Kowloon City",
    "GIHS": "MSKgwBPmtd",
    "Latitude": "22-18-44",
    "Longitude": "114-10-50",
    "Name_cn": "何文田公園",
    "Name_en": "Ho Man Tin Park",
    "Opening_hours_cn": "每日上午7時至晚上11時",
    "Opening_hours_en": "7 am to 11 pm daily",
    "Phone": "2762 7837",
    "Remarks_cn": "",
    "Remarks_en": ""
}
-}
data HardSurfaceSoccerPitches7aSideInfo = HardSurfaceSoccerPitches7aSideInfo
  { latitude :: Quantity DPlaneAngle Double,
    longitude :: Quantity DPlaneAngle Double
  }
  deriving stock (Show)

instance FromJSVal HardSurfaceSoccerPitches7aSideInfo where
  fromJSVal v = do
    let readFromWGS84Str = do
          deg <- (*~ degree) . fromIntegral @_ @Double <$> readPrec @Int
          '-' <- get
          min' <- (*~ arcminute) . fromIntegral <$> readPrec @Int
          '-' <- get
          sec <- (*~ arcsecond) . fromIntegral <$> readPrec @Int
          pure $ deg + min' + sec
        fromWGS84Str = \case
          Just t -> case [x | (x, "") <- readPrec_to_S readFromWGS84Str minPrec t] of
            [x] -> pure $ Just x
            _ -> pure Nothing
          Nothing -> pure Nothing
    mLat <- v ! "Latitude" >>= fromJSVal >>= fromWGS84Str
    mLng <- v ! "Longitude" >>= fromJSVal >>= fromWGS84Str
    pure $ HardSurfaceSoccerPitches7aSideInfo <$> mLat <*> mLng

instance Serialise HardSurfaceSoccerPitches7aSideInfo where
  encode (HardSurfaceSoccerPitches7aSideInfo lat lng) = encodeListLen 3 <> encodeWord 0 <> encode (lat /~ degree) <> encode (lng /~ degree)
  decode =
    (,) <$> decodeListLen <*> decodeWord >>= \case
      (3, 0) -> HardSurfaceSoccerPitches7aSideInfo <$> ((*~ degree) <$> decode) <*> ((*~ degree) <$> decode)
      _ -> fail "invalid HardSurfaceSoccerPitches7aSideInfo encoding"

listHardSurfaceSoccerPitches7aSideURI :: URI
listHardSurfaceSoccerPitches7aSideURI = [uri|https://www.lcsd.gov.hk/datagovhk/facility/facility-hssp7.json|]

listHardSurfaceSoccerPitches7aSide :: GenHaxl u w [HardSurfaceSoccerPitches7aSideInfo]
listHardSurfaceSoccerPitches7aSide = fetchCacheable (FetchJSON @[HardSurfaceSoccerPitches7aSideInfo] $ corsProxy listHardSurfaceSoccerPitches7aSideURI)
