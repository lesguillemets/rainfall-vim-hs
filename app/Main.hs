{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (concat)
import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Text as DCT
import Data.Text (Text, concat, pack)
import qualified Data.Text.IO as TI

import YOLP
import Vim

defaultPort :: IO Int
defaultPort = return 5678
main :: IO ()
main = do
    exitWatcher <- newEmptyMVar
    port <- defaultPort
    putStrLn $ "listening on " ++ show port
    forkTCPServer (serverSettings port "*") (run exitWatcher)
    takeMVar exitWatcher *> putStrLn "exit."

run ew appData =
    appSource appData $$ decode utf8
                      =$= conduit ew
                      =$= encode utf8
                      =$= appSink appData

conduit :: MVar () -> ConduitM Text Text IO ()
conduit ew = do
    msg <- (parseMsg <$>) <$> await
    case msg of
         Nothing -> liftIO $ do
             putStr "Connection closed."
             putMVar ew ()
         (Just (n,loc)) -> do
             x <- liftIO $ reportRainAt loc
             respond (n,x)
             conduit ew

respond (n,resp)
    = yield . concat $ [
        "[",
            pack . show $ n,
            ",",
            "\"",
            resp,
            "\"",
        "]"
        ]
