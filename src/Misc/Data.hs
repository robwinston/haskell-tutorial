module Misc.Data where
-- a collection of silly functions to practice haskell syntax ...
-- language features progressively employed as learned
-- sometimes a function is re-implemented accordingly

import Data.Char
import GHC.Exts

-- is char in string?
occurs :: Char -> String -> Bool
occurs _ [] = False
occurs c (s:ss)
  | c == s = True
  | otherwise = occurs c ss

-- index of first occurence, or length of string if not found
-- implemented without any 'base' functions
position :: Char -> String -> Int
position _ [] = 0
position c (s:ss)
 | c == s = 0
 | otherwise = 1 + (position c ss)

position2 :: Char -> String -> Int
position2 c s = length(takeWhile (/= c) s)

position3 :: Char -> String -> Int
position3 c s = head ((allpos c s) ++ [length s])

allpos c ss = [b | (a,b) <- (zip ss [0..(length ss)]), a == c]

--- test a list of position functions
testPoss :: [(Char -> String -> Int)] -> [Bool]
testPoss fs = (map testPos) fs

-- test a position function, finding each char in a generated string consisting of all but the last (to test not found as well)
-- the result should be an unbroken numeric sequence starting at 0
testPos :: (Char -> String -> Int) -> Bool
testPos f = and [a == b-1 | (a,b) <- zip poss (tail poss) ] && head poss == 0
  where poss = [f a ['0'..'y'] | a <- ['0'..'z']]

--  brute force pattern match
isWhiteSpaceBrute :: Char -> Bool
isWhiteSpaceBrute c
  | c == ' ' = True
  | c == '\n' = True
  | c == '\t' = True
  | otherwise = False


-- verified with "length (filter isWhiteSpace (map chr [0..255])) == 3"
isWhiteSpace :: Char -> Bool
isWhiteSpace c = isWhiteSpaceUsing [' ', '\t', '\n'] c

-- configurable
isWhiteSpaceUsing :: [Char] -> Char -> Bool
isWhiteSpaceUsing ws c = elem c ws

-- count words == strings delimited by whitespace
wc :: String -> Int
wc s = wcaux (' ' : s)
  where
    wcaux :: String -> Int
    wcaux [c] = 0
    wcaux (f:s:ss)
      | (isWhiteSpace f)  && not (isWhiteSpace s) = 1 + wcaux (s:ss)
      | otherwise = wcaux (s:ss)


myisLower :: Char -> Bool
myisLower c = c >= 'a' && c <= 'z'

myisUpper :: Char -> Bool
myisUpper c = c >= 'A' && c <= 'Z'

uc :: Char -> Char
uc c
  | myisLower c = chr ((ord c) - 32)
  | otherwise = c

-- Make first character uppercase
capitalise :: String -> String
capitalise "" = ""
capitalise (c:s) = (uc c):s

lc :: Char -> Char
lc c
  | myisUpper c = chr ((ord c) + 32)
  | otherwise = c


-- Make first character lowercase
uncapitalise :: String -> String
uncapitalise "" = ""
uncapitalise (c:s) = (lc c):s


allLower :: String -> String
allLower "" = ""
allLower s = map lc s

allUpper :: String -> String
allUpper "" = ""
allUpper s = map uc s

-- Silly tranform: capitalises any character not in supplied list
capExcept :: [String] -> String -> String
capExcept _ ""  = ""
capExcept [] s = capitalise s
capExcept exs s = if (elem s exs) then s else capitalise s

-- Simple currying of capExcept
capExceptThese :: [String] -> (String -> String)
capExceptThese exs = capExcept exs

-- inverse of capExcept with locally-defined helper function
capThese :: [Char] -> String -> String
capThese [] s = s
capThese _ [] = []
capThese toCap s = map capIf s
  where capIf :: Char -> Char
        capIf c
          | elem c toCap = uc c
          | otherwise = c


-- Brute force-ish transform of a list of strings into a single string separated by supplied delimiter
restring :: String -> [String] -> String
restring delim strings
 | length strings == 0 = ""
 | length strings == 1 = head strings
 | otherwise = (head strings) ++ delim ++ (restring delim (tail strings))


mytoTitle :: String -> String
mytoTitle s = mytoTitleCustom ["a", "the", "in", "and", "or", "is"] s


-- capitalise the first word always and all of the remaining words except those listed in exs
mytoTitleCustom :: [String] -> String -> String
mytoTitleCustom  _ "" = ""
mytoTitleCustom exs s = restring " "  ((capitalise (head theWords)) :  [capper w | w <- tail theWords])
  where
    theWords = words s
    capper = capExceptThese exs

-- make entire string lowercase, then turn it into a title
retitle :: String -> String
retitle "" = ""
retitle s = mytoTitle (allLower s)

-- reverse the capitalisation of a char
rc :: Char -> Char
rc c
  | myisUpper c = lc c
  | myisLower c = uc c
  | otherwise = c

-- reverse the capitalisation of a string
flipcap :: String -> String
flipcap s = map rc s


isVowel :: Char -> Bool
isVowel c =  elem  (toLower c) vowels
  where vowels = ['a', 'e', 'i', 'o', 'u']


-- translate a string of digits into a number
strToNum s = foldl nextDigit 0 s

nextDigit :: Int -> Char -> Int
nextDigit n c =  n * 10 + charToNum c

charToNum :: Char -> Int
charToNum c
  | ord '0' <= ord c &&  ord '9' >= ord c =  ord c - ord '0'


pigLatin :: String -> String
pigLatin "" = ""
pigLatin s = concat $ map pl (words s)
  where pl :: String -> String
        pl [] = []
        pl (x:y:xs)
          | length xs == 0 = [x] ++ "ay "
          | isUpper x = (toUpper y) : xs ++ [toLower x] ++ "ay "
          | otherwise = [y] ++ xs ++ [x] ++ "ay "
        pl (x:xs)
          | length xs == 0 = [x] ++ "ay "
          | isUpper x = (toUpper (head xs)) : [toLower x] ++ "ay "
          | otherwise = xs ++ [x] ++ "ay "


