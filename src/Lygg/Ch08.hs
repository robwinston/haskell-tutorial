module Lygg.Ch08 where

import Text.Read
import Data.Maybe

rpnCalc :: String -> Either String Double
rpnCalc [] = Left "Empty expression"
rpnCalc toEval = rpnEval (words toEval) []


rpnEval :: [String] -> [String] -> Either String Double
rpnEval [] (x:xs)
  | length xs > 0 = Left ("Unprocessed expressions: " ++ xs)
  | isNothing result = Left ("Invalid result: " ++ x)
  | otherwise =  Right $ fromJust result
  where result = readMaybe x :: Maybe Double
        operators = ["*", "/", "+", "-"]

