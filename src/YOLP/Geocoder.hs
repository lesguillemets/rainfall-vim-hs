module YOLP.Geocoder (
    geocode
    ) where

import Data.Text (Text)
import YOLP.Base (toRequest)
import YOLP.Geocoder.Query
import YOLP.Geocoder.Result
import Network.HTTP.Simple (httpLBS)
import Network.HTTP.Conduit (responseBody)

-- TODO : consider Either
geocode :: Text -> IO (Maybe GeoResp)
geocode q = do
    response <- httpLBS . toRequest $ baseQuery `withQuery` q
    return . getFirstCandidate . responseBody $ response
