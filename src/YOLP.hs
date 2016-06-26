{-# LANGUAGE OverloadedStrings #-}
-- YOLP: interface to
--  [Web Services by Yahoo! JAPAN](http://developer.yahoo.co.jp/about)
module YOLP (
    getRainAt,
    getRainAtWithPast,
    reportRainAt,
    Weather(..),
    YOLPError,
    ) where

import Prelude hiding (concat)
import Data.Text (Text, pack, concat)

import YOLP.Base (YOLPError(..))
import YOLP.Geocoder
import YOLP.Geocoder.Result (GeoResp(..))
import YOLP.Weather
import YOLP.Weather.Result (Weather(..), isForecast, isObserved)


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

-- TODO : make it better
report :: Either YOLPError (Text, [Weather]) -> Text
report (Left e) = pack . show $ e
report (Right (loc, ws))
    = concat [
        loc,
        ":",
        pack . show $ observedTotal,
        " mm rain in the past 2 hours/maximum of ",
        pack . show $ expectedMax,
        " mm/h rain in the next hour",
        " from ",
        time]
    where
        (observed, forecast) = span isObserved ws
        observeSpan = 2 / (fromIntegral . length $ observed)
        observedTotal =
            sum . map ((* observeSpan) . _rainfall) $ observed
        expectedMax = maximum . map _rainfall $ forecast
        f [] = "[]"
        f (h:_) = pack . drop 8 . _date $ h
        time = f . dropWhile ( (== 0) . _rainfall) $ forecast

reportRainAt :: Text -> IO Text
reportRainAt l = report <$> getRainAtWithPast l
