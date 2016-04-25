module HexCalc where

import Data.Maybe
import qualified Data.Map as DM


hexMap = (zip ((++) ['0'..'9'] ['A'..'F']) [0..15])

