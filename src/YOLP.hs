{-# LANGUAGE OverloadedStrings #-}
module YOLP (
    getRainAt,
    getRainAtWithPast,
    Weather(..),
    YOLPError) where

import Data.Text (Text)

import YOLP.Base (YOLPError(..))
import YOLP.Geocoder
import YOLP.Geocoder.Result (GeoResp(..))
import YOLP.Weather
import YOLP.Weather.Result (Weather)


-- TODO : Better abstraction
grBase :: ((Double,Double) -> IO (Maybe [Weather]))
       -> Text -> IO (Either YOLPError (Text, [Weather]))
grBase f loc = do
    location <- geocode loc
    case location of
         Nothing -> return $ Left GeocodeError
         Just l -> do
             weathers <- f . getCoords $ l
             case weathers of
                  Nothing -> return $ Left WeatherError
                  (Just ws) -> return $ Right (getLocName l, ws)

getRainAt :: Text -> IO (Either YOLPError (Text, [Weather]))
getRainAt = grBase getRain
getRainAtWithPast :: Text -> IO (Either YOLPError (Text, [Weather]))
getRainAtWithPast = grBase getRainWithPast
