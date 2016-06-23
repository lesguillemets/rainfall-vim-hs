module YOLP.Base where

import Network.HTTP.Conduit (Request)

class Requestable a where
    toRequest :: a -> Request

data Output = XmlOut | JsonOut
instance Show Output where
    show XmlOut = "xml"
    show JsonOut = "json"

data YOLPError = GeocodeError | WeatherError deriving (Show)-- FIXME
