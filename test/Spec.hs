{-# LANGUAGE OverloadedStrings #-}
import YOLP
import YOLP.Base
import YOLP.Weather.Query
import YOLP.Weather.Result.Internal
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

main :: IO ()
main = do
    (httpLBS . toRequest $ baseQuery `withCoords` (135,35))
        >>= mapM_ print . getWeathers . fromJust . decode . responseBody
    (httpLBS . toRequest $ G.baseQuery `G.withQuery` "明石市")
        >>= print . fromJust . getFirstCandidate . responseBody
    YOLP.getRainAtWithPast "京都市" >>=
        (\x -> case x of (Left _) -> print x
                         (Right (l,ws)) -> do
                             TI.putStrLn l
                             mapM_ print ws)

