module Vim (
    parseMsg
    ) where
import Data.Conduit
import Data.Conduit.Network
import Data.Conduit.Text as DCT
import Data.Text (Text)
import qualified Data.Text as T
import Control.Lens
import Data.Aeson.Lens

parseMsg :: Text -> (Int,Text)
parseMsg t = (n, msgBody)
    where
        Just n = t ^? nth 0 . _Integral
        Just msgBody = t ^? nth 1 . _String
