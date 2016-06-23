{-# LANGUAGE OverloadedStrings #-}
import YOLP
import YOLP.Base (toRequest)
import YOLP.Weather.Query
import YOLP.Weather.Result
import qualified YOLP.Geocoder.Query as G
import YOLP.Geocoder.Result (getFirstCandidate)
import Data.Text (Text)
import qualified Data.Text.IO as TI
import Network.HTTP.Simple (httpLBS)
import Network.HTTP.Conduit (responseBody)
import Data.Maybe (fromJust)

main :: IO ()
main = do
    (httpLBS . toRequest $ G.baseQuery `G.withQuery` "明石市")
        >>= print . fromJust . getFirstCandidate . responseBody
    YOLP.getRainAtWithPast "京都市" >>=
        (\x -> case x of (Left _) -> print x
                         (Right (l,ws)) -> do
                             TI.putStrLn l
                             mapM_ print ws)
