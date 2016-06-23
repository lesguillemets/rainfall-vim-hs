{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module YOLP.Weather.Query (
    baseQuery,
    withCoords,
    withOutput,
    withDate,
    withPast,
    withInterval,
    Past (..),
    Output (..)
    ) where

import qualified Network.HTTP.Conduit as HC
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

import YOLP.Base
import Secrets (myAppID)

Just apiTarget = HC.parseUrl "http://weather.olp.yahooapis.jp/v1/place"

data WeatherQuery
    = WeatherQuery { appID    :: ByteString
                   , coords   :: (Double,Double)
                   , output   :: Maybe Output
                   , date     :: Maybe ByteString
                   , past     :: Maybe Past
                   , interval :: Maybe Int
                   }

withCoords :: WeatherQuery -> (Double,Double) -> WeatherQuery
withCoords q c = q{coords = c}
withOutput :: WeatherQuery -> Output -> WeatherQuery
withOutput q o = q{output = Just o}
withDate :: WeatherQuery -> ByteString -> WeatherQuery
withDate q d = q{date = Just d}
withPast :: WeatherQuery -> Past -> WeatherQuery
withPast q p = q{past = Just p}
withInterval :: WeatherQuery -> Int -> WeatherQuery
withInterval q i = q{interval = Just i}


instance Requestable WeatherQuery where
    toRequest wq@WeatherQuery{..}
        = HC.setQueryString [
            ("appID", Just appID),
            ("coordinates",
                Just . BC.pack . (\(n,m) -> show n  ++ "," ++ show m) $ coords
                ),
            ("output", (BC.pack . show) <$> output),
            ("date" , date),
            ("past", (BC.pack . show) <$> past),
            ("interval", (BC.pack . show) <$> interval)
        ] $ apiTarget {
                HC.method = "GET",
                HC.requestHeaders = [
                    ("user-agent", "Haskell-HTTP-Conduit")
                    ]
                }

baseQuery :: WeatherQuery
baseQuery = WeatherQuery {
    appID    = myAppID,
    coords   = (0,0),
    output   = Just JsonOut,
    date     = Nothing,
    past     = Nothing,
    interval = Nothing }

data Past = NoPast | OneHour | TwoHours
instance Show Past where
    show NoPast = "0"
    show OneHour = "1"
    show TwoHours = "2"

