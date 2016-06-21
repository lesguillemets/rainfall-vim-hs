import YOLP.Base
import YOLP.Weather
import Network.HTTP.Simple (httpLBS)
main :: IO ()
main = do
    (httpLBS . toRequest $ baseQuery `withCoords` (135,35)) >>= print
