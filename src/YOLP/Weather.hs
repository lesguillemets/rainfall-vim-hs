module YOLP.Weather (getRain, getRainWithPast) where

import Data.Text (Text)
import YOLP.Base (Requestable, toRequest)
import YOLP.Weather.Query
import YOLP.Weather.Result
import Network.HTTP.Simple (httpLBS)
import Network.HTTP.Conduit (responseBody, Request)
import Data.Aeson (decode)
--
-- TODO : consider Either
report :: Requestable r => r -> IO (Maybe [Weather])
report req = do
    response <- httpLBS . toRequest $ req
    return . sh $ getWeathers <$> (decode . responseBody $ response)
    where sh Nothing = Nothing
          sh (Just []) = Nothing
          sh (Just (h:_)) = Just h

getRain :: (Double, Double) -> IO (Maybe [Weather])
getRain l = report $ baseQuery `withCoords` l

getRainWithPast :: (Double, Double) -> IO (Maybe [Weather])
getRainWithPast l = report $ baseQuery `withCoords` l `withPast` TwoHours
