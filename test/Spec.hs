{-# LANGUAGE OverloadedStrings #-}
import YOLP
import YOLP.Base
import YOLP.Weather.Query
import YOLP.Weather.Result
import qualified YOLP.Geocoder.Query as G
import YOLP.Geocoder.Result
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text.IO as TI
import Network.HTTP.Simple (httpLBS)
import Network.HTTP.Conduit (responseBody)
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (fromJust)
-- TODO : check how to do this
assertJust :: String -> Maybe a -> IO ()
assertJust msg (Just x) = putStrLn ("success:" ++ msg)
assertJust msg Nothing = putStrLn ("fail on" ++ msg) *> exitFailure
resultInfoText =  BLC.unlines [
        "{\"Count\":1,\"Total\":1,\"Start\":1,\"Status\":200,",
         "\"Latency\":0.003837,\"Description\":\"\",",
         "\"Copyright\":\"(C) Yahoo Japan Corporation.\"}"
         ]
weatherListText = BLC.unlines [
    "{\"Weather\":[",
    "{\"Type\":\"observation\",",
    "\"Date\":\"201606211835\",",
    "\"Rainfall\":0.00},",
    "{\"Type\":\"observation\",",
    "\"Date\":\"201606211835\",",
    "\"Rainfall\":0.00",
    "}]}"]

geoText = BLC.unlines [
    "{\"Type\":\"point\",",
     "\"Coordinates\":\"139.73229,",
     "35.663613\" }"
    ]

propertyText = BLC.unlines [
    "{\"WeatherAreaCode\":4410," ,
    "\"WeatherList\" : ",
    weatherListText,
    "}"
    ]

main :: IO ()
main = do
    (assertJust "obs" :: Maybe WType -> IO ()) . decode $ "\"observation\""
    (assertJust "rsult" :: Maybe ResultInfo -> IO ()) . decode $ resultInfoText
    (assertJust "wlist":: Maybe WeatherList -> IO ()) . decode $ weatherListText
    (assertJust "prop" :: Maybe Property -> IO ()) . decode $ propertyText
    (assertJust "geo" :: Maybe Geometry -> IO ()) . decode $ geoText
    (httpLBS . toRequest $ baseQuery `withCoords` (135,35))
        >>= mapM_ print . getWeathers . fromJust . decode . responseBody
    (httpLBS . toRequest $ G.baseQuery `G.withQuery` "明石市")
        >>= print . fromJust . getFirstCandidate . responseBody
    YOLP.getRainAtWithPast "京都市" >>=
        (\x -> case x of (Left _) -> print x
                         (Right (l,ws)) -> do
                             TI.putStrLn l
                             mapM_ print ws)

