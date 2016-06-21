module YOLP.Base where

import Network.HTTP.Conduit (Request)

class Requestable a where
    toRequest :: a -> Request

