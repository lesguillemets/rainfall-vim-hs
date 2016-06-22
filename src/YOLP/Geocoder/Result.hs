{-# LANGUAGE OverloadedStrings #-}
module YOLP.Geocoder.Result (
    GeoResp(..),
    getFirstCandidate
    ) where

import Control.Lens
import Data.Aeson.Lens
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T

data GeoResp = GeoResp {
    getLocName :: Text,
    getCoords :: (Double,Double)
}

instance Show GeoResp where
    show (GeoResp name coords) = T.unpack name ++ show coords

parseCoords :: Text -> Maybe (Double, Double)
parseCoords s =
    let xs = map (read . T.unpack) . T.split (== ',') $ s
        in
        case xs of
             (lat:lng:_) -> Just (lat,lng)
             _ -> Nothing

getFirstCandidate :: ByteString ->  Maybe GeoResp
getFirstCandidate txt = do
    firstCand <- txt ^? key "Feature" . nth 0
    name <- firstCand ^? key "Name" . _String
    -- TODO : probably a better way
    coords <- (firstCand ^? key "Geometry")
                >>= (^? key "Coordinates" . _String)
                    >>= parseCoords
    return $ GeoResp name coords
