{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module YOLP.Geocoder.Query (
    baseQuery,
    withQuery
    ) where

import qualified Network.HTTP.Conduit as HC
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Char (toLower)

import YOLP.Base
import Secrets (myAppID)

Just apiTarget = HC.parseUrl
    "http://geo.search.olp.yahooapis.jp/OpenLocalPlatform/V1/geoCoder"
data GeocoderQuery =
    GeocoderQuery { appID :: ByteString,
                    query :: Maybe Text,
                    ei :: Maybe Encoding,
                    lat :: Maybe Double,
                    lon :: Maybe Double,
                    bbox :: Maybe (Double, Double, Double, Double),
                    datum :: Maybe Datum,
                    ac :: Maybe AddressCode,
                    al :: Maybe Int,
                    ar :: Maybe AR,
                    recursive :: Maybe Bool,
                    sort :: Maybe Sort,
                    excludePrefecture :: Maybe Bool,
                    excludeSeireishi :: Maybe Bool,
                    start :: Maybe Int,
                    page :: Maybe Int,
                    results :: Maybe Int,
                    output :: Maybe Output
                  }

withQuery :: GeocoderQuery -> Text -> GeocoderQuery
withQuery q t = q{query=Just t}

-- way too tedious
instance Requestable GeocoderQuery where
    toRequest gq@GeocoderQuery{..}
        = HC.setQueryString [
            ("appID", Just appID),
            ("query", TE.encodeUtf8 <$> query),
            ("ei", (BC.pack . show) <$> ei),
            ("lat", (BC.pack . show) <$> lat),
            ("lon", (BC.pack . show) <$> lon),
            ("bbox", (BC.pack . show) <$> bbox),
            ("datum", (BC.pack . show) <$> datum),
            ("ac", (BC.pack . show) <$> ac),
            ("al", (BC.pack . show) <$> al),
            ("ar", (BC.pack . show) <$> ar),
            ("recursive", (BC.pack . map toLower . show) <$> recursive),
            ("sort", (BC.pack . map toLower . show) <$> sort),
            ("exclude_prefecture",
                (BC.pack . map toLower . show) <$> excludePrefecture),
            ("exclude_seireishi",
                (BC.pack . map toLower . show) <$> excludeSeireishi),
            ("start", (BC.pack . show) <$> start),
            ("page", (BC.pack . show) <$> page),
            ("results", (BC.pack . show) <$> results),
            ("output", (BC.pack . show) <$> output)
        ] $ apiTarget {
                HC.method = "GET",
                HC.requestHeaders = [
                    ("user-agent", "Haskell-HTTP-Conduit")
                    ]
                }

baseQuery :: GeocoderQuery
baseQuery = GeocoderQuery {
    appID = myAppID,
    query = Just "",
    ei = Just EnUTF8,
    lat = Nothing,
    lon = Nothing,
    bbox = Nothing,
    datum = Nothing,
    ac = Nothing,
    al = Nothing,
    ar = Nothing,
    recursive = Nothing,
    sort = Nothing,
    excludePrefecture = Nothing,
    excludeSeireishi = Nothing,
    start = Nothing,
    page = Nothing,
    results = Nothing,
    output = Nothing
}


type AddressCode = ByteString
data Encoding = EnUTF8 | EnEUCJP | EnSJIS
instance Show Encoding where
    show EnUTF8 = "UTF-8"
    show EnEUCJP = "EUC-JP"
    show EnSJIS = "SJIS"

data Datum = WGS | TKY
instance Show Datum where -- TODO : map toLower . derivedShow
    show WGS = "wgs"
    show TKY = "tky"

data AR = ARGe | ARLe | AREq
instance Show AR where
    show ARGe = "ge"
    show ARLe = "le"
    show AREq = "eq"

data Sort = SScore | SDist | SKana | SRevKana | SAddress | SAddress2
instance Show Sort where
    show SScore = "score"
    show SDist = "dist"
    show SKana = "kana"
    show SRevKana = "-kana"
    show SAddress = "address"
    show SAddress2 = "address2"
