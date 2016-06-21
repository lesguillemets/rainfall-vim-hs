module Helper (
    headCap,
    headLow
    ) where

import Data.Char (toUpper, toLower)


headCap :: String -> String
headCap [] = []
headCap (c:r) = toUpper c : r
headLow :: String -> String
headLow [] = []
headLow (c:r) = toLower c : r
