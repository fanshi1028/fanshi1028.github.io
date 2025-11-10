{-# LANGUAGE QuasiQuotes #-}

-- NOTE: http://data.gov.hk/tc/help/api-spec
module Dashboard.DataSource.DataGovHK.URI
  ( appDataGovlistFilesURI,
    appDataGovlistFilesVersionsURI,
    appDataGovGetFileURI,
    appDataGovGetSchemaURI,
    appDataGovGetDataDictionaryURI,
    withDefaultPaging,
  )
where

import Data.ByteString.Builder
import Data.Text (StrictText, pack, show)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding
import Data.Time.Calendar
import Data.Time.Format
import Network.HTTP.Types
import Network.URI
import Network.URI.Static
import Numeric.Natural
import Prelude hiding (show)

data Paging = Paging
  { _itemPerPage :: Natural,
    _page :: Natural
  }

withDefaultPaging :: Natural -> Paging
withDefaultPaging = Paging 10

renderQueryTextAsString :: QueryText -> String
renderQueryTextAsString = unpack . decodeUtf8 . toLazyByteString . renderQueryText True

fmtDayYmd :: Day -> StrictText
fmtDayYmd = pack . formatTime defaultTimeLocale "%Y%m%d"

-------------------------------------------------------------------------------------------
-- 取回在start和end指定日期以內並符合category，provider和 format參數的檔案清單。         --
--                                                                                       --
-- 每次最多有max數目的結果返回，首skip數目的結果會被略去。利用這兩個參數可達到分頁用途。 --
-------------------------------------------------------------------------------------------
appDataGovlistFilesURI :: Day -> Day -> Paging -> URI
appDataGovlistFilesURI start end (Paging itemPerPage page) =
  [uri|https://app.data.gov.hk/v1/historical-archive/list-files|]
    { uriQuery =
        renderQueryTextAsString
          [ (pack "start", Just $ fmtDayYmd start),
            (pack "end", Just $ fmtDayYmd end),
            (pack "max", Just $ show itemPerPage),
            (pack "skip", Just . show $ page * itemPerPage)
            -- NOTE: 類別識別碼, 例如: information-technology-and-broadcasting。
            -- (pack "category", Nothing),
            -- NOTE: 數據提供機構識別碼, 例如: hk-dpo。
            -- (pack "provider", Nothing),
            -- NOTE: 檔案格式，以檔案的副檔名，例如: xls。
            -- (pack "format", Nothing),
            -- NOTE: 關鍵詞搜尋。只有與數據集／資源名稱匹配的結果會返回。
            -- (pack "search", Nothing),
            -- NOTE: 排序。 有效選項為: dataset-en，dataset-tc，dataset-sc，resource-en，resource-tc，resource-sc 和 url。預設排序為url。.
            -- (pack "order", Nothing)
          ]
    }

-----------------------------------------------------------------------------
-- 取回在日期（start 和 end中提供）以內的檔案（url中提供）的歷史版本清單。 --
-- 只有首10,000個結果將會返回。                                            --
-----------------------------------------------------------------------------
appDataGovlistFilesVersionsURI :: StrictText -> Day -> Day -> URI
appDataGovlistFilesVersionsURI url start end =
  [uri|https://app.data.gov.hk/v1/historical-archive/list-files-versions|]
    { uriQuery =
        renderQueryTextAsString
          [ -- NOTE: 檔案網址，網址可從歷史檔案文件列表應用程式介面結果中找到。
            (pack "url", Just url),
            (pack "start", Just $ fmtDayYmd start),
            (pack "end", Just $ fmtDayYmd end)
          ]
    }

-----------------------------------------------
-- 取回基於time的檔案（url中提供）歷史版本。 --
-----------------------------------------------
appDataGovGetFileURI :: StrictText -> Day -> URI
appDataGovGetFileURI url start =
  [uri|https://app.data.gov.hk/v1/historical-archive/get-file|]
    { uriQuery =
        renderQueryTextAsString
          [ -- NOTE: 檔案網址，網址可從歷史檔案文件列表應用程式介面結果中找到。
            (pack "url", Just url),
            (pack "time", Just $ fmtDayYmd start)
          ]
    }

--------------------------------------------------------------
-- https://app.data.gov.hk/v1/historical-archive/get-schema --
--------------------------------------------------------------
appDataGovGetSchemaURI :: StrictText -> Day -> URI
appDataGovGetSchemaURI url start =
  [uri|https://app.data.gov.hk/v1/historical-archive/get-schema|]
    { uriQuery =
        renderQueryTextAsString
          [ -- NOTE: 檔案網址，網址可從歷史檔案文件列表應用程式介面結果中找到。
            (pack "url", Just url),
            (pack "time", Just $ fmtDayYmd start)
          ]
    }

---------------------------------------------------
-- 取回基於date的數據字典（url中提供）歷史版本。 --
---------------------------------------------------
appDataGovGetDataDictionaryURI :: StrictText -> Day -> URI
appDataGovGetDataDictionaryURI url start =
  [uri|https://app.data.gov.hk/v1/historical-archive/get-data-dictionary|]
    { uriQuery =
        renderQueryTextAsString
          [ -- NOTE: 檔案網址，網址可從歷史檔案文件列表應用程式介面結果中找到。
            (pack "url", Just url),
            (pack "date", Just $ fmtDayYmd start)
          ]
    }
