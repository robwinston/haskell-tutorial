module Lygg.Ch07 where

import Lygg.Geometry
import Misc.Sieve
import Data.List
import Data.Maybe
import qualified Data.Map as Map


-- record type
data Person = Person
               { firstName :: String
               , lastName :: String
               , age :: Int
               }
               deriving (Eq, Show, Read)

-- must supply all fields, to use 'read  mysteryDude :: Person
mysteryDude = "Person { firstName = \"Michael\", lastName = \"Diamond\", age = 43 }"


-- type constructor
data Vector a = Vector a a a deriving (Show)

-- infix in function declarations
vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
            deriving (Eq, Ord, Show, Read, Bounded, Enum)

allDays = [minBound .. maxBound] :: [Day]

-- type aliases
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type AssocList k v = [(k, v)]

assocLookup :: String -> AssocList String Int -> Maybe Int
assocLookup key al
  | isJust entry = Just $ snd $ fromJust $ entry
  | otherwise = Nothing
  where entry = find (\(ky,vl) -> ky == key) al


-- illustrating Either
data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
      Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
      Just (state, code) -> if state /= Taken
                            then Right code
                            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken, "ZD39I"))
    ,(101,(Free, "JAH3I"))
    ,(103,(Free, "IQSA9"))
    ,(105,(Free, "QOTSA"))
    ,(109,(Taken, "893JJ"))
    ,(110,(Taken, "99292"))
    ]

{-
ghci> map (\ln -> lockerLookup ln lockers) [100..103]
[Left "Locker 100 is already taken!",Right "JAH3I",Left "Locker 102 doesn't exist!",Right "IQSA9"]
ghci>
-}


--data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- as record syntax ...
-- data List a = Empty | Cons { listHead :: a, listTail :: List a}
--    deriving (Show, Read, Eq, Ord)

{-
ghci> let a = Empty
ghci> let b = Cons "A" a
ghci> let c = Cons "B" b
ghci> c
Cons {listHead = "B", listTail = Cons {listHead = "A", listTail = Empty}}
ghci>
-}


-- infix with a fixity ...
-- A fixity states how tightly the operator binds and whether it’s left-associative or right-associative

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5  ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)


{-

ghci> let a = 3 :-: 4 :-: 5 :-: Empty
ghci> let b = 6 :-: 7 :-: Empty
ghci> a ^++ b
3 :-: (4 :-: (5 :-: (6 :-: (7 :-: Empty))))
ghci>

-}



data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right


{-
ghci> let primesTree = foldr treeInsert EmptyTree $ sieve 20
ghci> map (\n -> treeElem n primesTree) $ take 10 $ drop 15 $ sieve 30
[True,True,True,True,True,False,False,False,False,False]
ghci>
-}


-- emulate javascript's concept of Bool for some stuff ...

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    -- id function simply returns the parameter
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
    if yesno yesnoVal
        then yesResult
        else noResult

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

