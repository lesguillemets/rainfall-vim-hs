{-# LANGUAGE TemplateHaskell #-}
module YOLP.Weather.Result where

import Data.Aeson
import Data.Aeson.TH
import Helper (headCap, headLow)

data ResultInfo = ResultInfo
                { _count :: Int,
                  _total :: Int,
                  _start :: Int,
                  _status :: Int,
                  _latency :: Double,
                  _description :: String,
                  _copyright :: String}
                deriving (Show, Eq)
$(deriveJSON defaultOptions{
    fieldLabelModifier = headCap . drop 1
    } ''ResultInfo)

data Geometry = Geometry { _geoType :: String
                         , _geoCoordinates :: String -- FIXME
                         } deriving (Show, Eq)
$(deriveJSON defaultOptions{
    fieldLabelModifier = drop 4
    } ''Geometry)

data WType = Observation | Forecast deriving (Show, Read, Eq)
$(deriveJSON defaultOptions {constructorTagModifier = headLow} ''WType)

data Weather = Weather { _type :: WType
                       , _date :: String
                       , _rainfall :: Double
                       } deriving (Show,Eq)
$(deriveJSON defaultOptions{
    fieldLabelModifier = headCap . drop 1
    } ''Weather)

-- TODO : how to use newtype?
data WeatherList = WeatherList {_getWeathers :: [Weather]} deriving (Show, Eq)
$(deriveJSON defaultOptions{
    fieldLabelModifier = const "Weather"
    } ''WeatherList)

data Property
    = Property { _weatherAreaCode :: Int
               , _weatherList :: WeatherList
               } deriving (Show,Eq)
$(deriveJSON defaultOptions{
    fieldLabelModifier = headCap . drop 1
    } ''Property)

data Feature
    = Feature { _id :: String
              , _name :: String
              , _geometry :: Geometry
              , _property :: Property
              } deriving (Show, Eq)
$(deriveJSON defaultOptions{
    fieldLabelModifier = headCap . drop 1
    } ''Feature)

data WeatherResult = WeatherResult { _resultInfo :: ResultInfo
                                   , _feature :: [Feature]
                                   } deriving (Show, Eq)
$(deriveJSON defaultOptions{
    fieldLabelModifier = headCap . drop 1
    } ''WeatherResult)

getWeathers :: WeatherResult -> [[Weather]]
getWeathers = map (_getWeathers . _weatherList . _property) .  _feature
