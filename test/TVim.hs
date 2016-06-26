{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Vim
import Data.Text (Text)

main :: IO ()
main = hspec $ do
    describe "parseMsg" $ do
        it "parses nicely" $ do
            parseMsg "[1,\"Hello!\"]" `shouldBe` (1,"Hello!")
