{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import YOLP.Weather.Result.Internal
import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import Codec.Binary.UTF8.String (encodeString)
import qualified Data.ByteString.Lazy.Char8 as BLC


main :: IO ()
main = hspec $ do
    describe "ResultInfo" $ do
        it "works correctly" $ do
            decode resultInfoText `shouldBe` Just thisResultInfo
    
    describe "Geometry" $
        it "works fine" $ do
            decode geometryText `shouldBe` Just thisGeometry
    
    describe "Weather" $
        it "works fine" $ do
            decode weatherText `shouldBe` Just thisWeather
    
    describe "WeatherList" $
        it "works fine" $ do
            decode weatherListText `shouldBe` Just thisWeatherList
    
    describe "Property" $
        it "works fine" $ do
            decode propertyText `shouldBe` Just thisProperty
    
    describe "Feature" $ do
        it "works fine" $ do
            decode featureText `shouldBe` Just thisFeature
    
    describe "WeatherResult" $ do
        it "works fine with all that" $ do
            decode resultText `shouldBe` Just thisResult

thisResultInfo :: ResultInfo
thisResultInfo =
    ResultInfo 1 1 1 200 0.003837 "" "(C) Yahoo Japan Corporation."
resultInfoText :: ByteString
resultInfoText =  BLC.unwords [
        "{\"Count\":1,\"Total\":1,\"Start\":1,\"Status\":200,",
         "\"Latency\":0.003837,\"Description\":\"\",",
         "\"Copyright\":\"(C) Yahoo Japan Corporation.\"}"
         ]

thisGeometry :: Geometry
thisGeometry = Geometry "point" "139.73229, 35.663613"
geometryText :: ByteString
geometryText = BLC.unwords [
    "{\"Type\":\"point\",",
     "\"Coordinates\":\"139.73229,",
     "35.663613\" }"
    ]

thisWeather :: Weather
thisWeather = Weather Observation "201606211835" 0
weatherText :: ByteString
weatherText = BLC.unwords [
    "{\"Type\":\"observation\",",
    "\"Date\":\"201606211835\",",
    "\"Rainfall\":0.00}"
    ]

thisWeatherList :: WeatherList
thisWeatherList = WeatherList [thisWeather,
                               Weather Forecast "201606211935" 0]
weatherListText :: ByteString
weatherListText = BLC.unwords [
    "{\"Weather\":[",
    weatherText,
    ",",
    "{\"Type\":\"forecast\",",
    "\"Date\":\"201606211935\",",
    "\"Rainfall\":0.00",
    "}]}"]

thisProperty :: Property
thisProperty = Property 4410 thisWeatherList
propertyText :: ByteString
propertyText = BLC.unwords [
    "{\"WeatherAreaCode\":4410," ,
    "\"WeatherList\" : ",
    weatherListText,
    "}"
    ]

thisFeature :: Feature
thisFeature = Feature "201606211835_139.73229_35.663613"
                      "地点(139.73229,35.663613)"
                      thisGeometry
                      thisProperty
featureText :: ByteString
featureText = BLC.unwords [
        "{",
          "\"Id\": \"201606211835_139.73229_35.663613\",",
          BLC.pack . encodeString $
            "\"Name\": \"地点(139.73229,35.663613)\", ",
          "\"Geometry\":", geometryText,
          ",",
          "\"Property\":", propertyText,
        "}"
    ]

thisResult :: WeatherResult
thisResult = WeatherResult thisResultInfo [thisFeature]
resultText :: ByteString
resultText = BLC.unwords [
    "{",
      "\"ResultInfo\":",  resultInfoText, ",",
      "\"Feature\": [", featureText , "]}"
    ]
