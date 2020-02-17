{-# LANGUAGE InstanceSigs #-} -- allows us to write signatures in instance declarations

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2

import Prelude hiding (Maybe(..), maybe, lookup)

-- TODO: PLEASE HLINT (maybe show hlint?)
-- TODO: records? show
-- data Person = Person String Int

-- TODO: Maybe?
-- Maybe as "proof" vs Bool

data Maybe a
  = Nothing
  | Just a
  deriving Show

-- EXERCISE: Equality on Maybes
-- Lift equality
instance (Eq a) => Eq (Maybe a) where
  Nothing == Nothing = True
  Just x == Just y = x == y
  _ == _ = False

-- EXERCISE: Deconstruct a list if possible
--
-- EXAMPLES:
-- safeUncons [] == Nothing
-- safeUncons [10,13,12] == Just (10, [13,12])
safeUncons :: [a] -> Maybe (a, [a])
safeUncons []     = Nothing
safeUncons (x:xs) = Just (x, xs)

-- EXERCISE: Safe integer division
-- We can't divide by 0, so we don't always return a result.
-- Return both the quotient and the remainder
safeDiv :: Int -> Int -> Maybe (Int, Int)
safeDiv _ 0 = Nothing
safeDiv n m = Just (n `div` m, n `rem` m)

-- EXERCISE: Strip a prefix from a string
-- But the second string doesn't always contain the first as a prefix,
-- so we want to be able to fail.
--
-- EXAMPLES:
-- stripPrefix "foo" "foobar" == Just "bar"
-- stripPrefix "mgla" "kpop" == Nothing
stripPrefix :: String -> String -> Maybe String
stripPrefix []     ys = Just ys
stripPrefix _      [] = Nothing
stripPrefix (x:xs) (y:ys)
  | x /= y     = Nothing
  | isJust rec = rec
  | otherwise  = Nothing
  where rec = stripPrefix xs ys
        isJust :: Maybe a -> Bool
        isJust Nothing = False
        isJust _       = True

-- EXERCISE: Lookup in an associative list
--
-- EXAMPLES:
-- lookup 5 [(10, 'a'), (5,'c')] == Just 'c'
-- lookup 13 [(10, 'a'), (5,'c')] == Nothing
lookup :: Eq k => k -> [(k, v)] -> Maybe v
lookup _ [] = Nothing
lookup n ((m,v):xs) = if n == m then Just v else lookup n xs

-- EXERCISE: Fallback
--
-- EXAMPLES:
-- fromMaybe 5 Nothing == 5
-- fromMaybe 5 (Just 10) == 10
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just x) = x

-- EXERCISE: Total deconstruction of a Maybe
-- Sometimes useful instead of pattern matching
-- or with higher order functions
--
-- EXAMPLES:
-- maybe 5 succ (Just 10) == 11
-- maybe 5 succ Nothing == 5
maybe :: b -> (a -> b) -> Maybe a -> b
maybe x _ Nothing  = x
maybe _ f (Just x) = f x

-- EXERCISE: Convert a maybe to a list
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

-- EXERCISE: Keep the first element of a list, if there are any
listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

-- EXERCISE: Sum all the values inside the maybe
--
-- EXAMPLES:
-- sumMaybe Nothing == 0
-- sumMaybe (Just 5) == 5
sumMaybe :: Num a => Maybe a -> a
sumMaybe m = fromMaybe 0 m

-- EXERCISE: Apply a function on a value, if there is any
-- Note that this function in base does a different thing!
--
-- EXAMPLES:
-- mapMaybe succ Nothing == Nothing
-- mapMaybe succ (Just 41) == Just 42
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f = maybe Nothing (Just . f)

-- EXERCISE: Folding a Maybe
--
-- EXAMPLES:
-- foldMaybe (Just [1,2,3]) == [1,2,3]
-- foldMaybe (Nothing :: Maybe [Int]) == []
foldMaybe :: Monoid a => Maybe a -> a
foldMaybe = fromMaybe mempty

-- EXERCISE: Analogue of foldMap for lists
foldMapMaybe :: Monoid b => (a -> b) -> Maybe a -> b
foldMapMaybe f = maybe mempty f

-- EXERCISE: Get all the Justs from a list
--
-- EXAMPLES:
-- catMaybes [Just 5, Nothing, Just 10] == [5, 10]
catMaybes :: [Maybe a] -> [a]
--catMaybes = concatMap maybeToList
--catMaybes = concatMap $ foldMapMaybe (:[])
catMaybes xs = [x | Just x <- xs]

-- EXERCISE : Map all the values of a list with possible failure
-- and get back all the successful results
--
-- This is what Data.Maybe.mapMaybe does.
-- Contrast this with filter!
--
-- mapListMaybe safeUncons [[], [1,2], [], [2,3]] == [(1, [2]), (2, [3])]
mapListMaybe :: (a -> Maybe b) -> [a] -> [b]
mapListMaybe f = catMaybes . map f

-- EXERCISE: "Adapter" to convert (a -> Bool) functions to Maybe returning ones
--
-- EXAMPLES:
-- (onBool even) 3 == Nothing
-- (onBool even) 3 == Just 3
onBool :: (a -> Bool) -> a -> Maybe a
onBool f x = if f x then Just x else Nothing

-- EXERCISE: Conditional execution
-- (note how this looks like flip concatMap :: [a] -> (a -> [b]) -> [b] !)
--
-- EXAMPLES:
--xs1 = [(2, "k"), (3, "e"), (42, "k")]
--x1 = lookup 2 xs1 `ifJust` (\x ->
--     lookup 3 xs1 `ifJust` (\y ->
--     lookup 42 xs1 `ifJust` (\z ->
--     Just $ x ++ y ++ z)))
---- x1 == Just "kek"
----
--xs2 = [(13, "k"), (3, "e"), (42, "k")]
--x2 = lookup 2 xs2 `ifJust` (\x ->
--     lookup 3 xs2 `ifJust` (\y ->
--     lookup 42 xs2 `ifJust` (\z ->
--     Just $ x ++ y ++ z)))
---- x2 == Nothing
--
ifJust :: Maybe a -> (a -> Maybe b) -> Maybe b
ifJust = flip $ maybe Nothing

-- Induced Semigroup by a
instance Semigroup a => Semigroup (Maybe a) where
  (<>) :: Maybe a -> Maybe a -> Maybe a
  (<>) = undefined

-- Maybe lifts any Semigroup to a Monoid!
instance Semigroup a => Monoid (Maybe a) where
  mempty :: Maybe a
  mempty = undefined

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving Show

-- EXERCISE: Equality on trees
instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) = undefined

-- Depending on how you write your folds you will get
-- differente Tree traversals -
-- * root left right
-- * left root right
-- * etc..
-- Let's implement "left root right" traversal everywhere in our functions
-- So we want treeToList (Node 5 (Tree 2 Empty Empty) (Tree 7 Empty Empty))
-- to be [2,5,7]

-- EXERCISE: Flattening a tree
--
-- EXAMPLES:
-- treeToList (Node 5 (Tree 2 Empty Empty) (Tree 7 Empty Empty)) == [2, 5, 7]
treeToList :: Tree a -> [a]
treeToList = undefined

-- EXERCISE: Insert into a tree, by using the ordering on a to guide you
--
-- EXAMPLES:
-- insertOrdered 5 Empty == Node 5 Empty Empty
-- insertOrdered 5 (Node 10 Empty Empty) == Node 10 (Node 5 Empty Empty) Empty
-- insertOrdered 5 (Node 3 Empty Empty) == Node 10 Empty (Node 5 Empty Empty)
insertOrdered :: Ord a => a -> Tree a -> Tree a
insertOrdered = undefined

-- EXERCISE: Create a (binary search) Tree from a list, using insertOrdered
--
-- EXAMPLES:
-- listToTree [1..10] == Node 10 (Node 9 (Node 8 (Node 7 (Node 6 (Node 5 (Node 4 (Node 3 (Node 2 (Node 1 Empty Empty) Empty) Empty) Empty) Empty) Empty) Empty) Empty) Empty) Empty
-- listToTree [1,10,2,9,3,8] == Node 8 (Node 3 (Node 2 (Node 1 Empty Empty) Empty) Empty) (Node 9 Empty (Node 10 Empty Empty))
listToTree :: Ord a => [a] -> Tree a
listToTree = undefined

-- EXERCISE: Summing a tree
--
-- EXAMPLES:
-- sumTree (Node 5 (Tree 2 Empty Empty) (Tree 7 Empty Empty)) == 14
sumTree :: Num a => Tree a -> a
sumTree = undefined

-- EXERCISE: Mapping over a tree
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = undefined

-- EXERCISE: Does the entire tree "satisfy" a predicate?
--(Node 5 (Tree 2 Empty Empty) (Tree 7 Empty Empty))
allTree :: (a -> Bool) -> Tree a -> Bool
allTree = undefined

-- EXERCISE: Is an element present in a tree?
--
-- elemTree 5 (listToTree [1..10]) == True
-- elemTree 42 (listToTree [1..10]) == False
elemTree :: Eq a => a -> Tree a -> Bool
elemTree = undefined

-- EXERCISE: Is there an element satisfying a predicate?
--
-- elemTree even (listToTree [1..10]) == Just 2 -- or something else idk
-- elemTree (>20) (listToTree [1..10]) == Nothing
findPred :: (a -> Bool) -> Tree a -> Maybe a
findPred = undefined

-- EXERCISE: Fold a tree
foldTree :: Monoid a => Tree a -> a
foldTree = undefined

-- EXERCISE: ..while first mapping
foldMapTree :: Monoid b => (a -> b) -> Tree a -> b
foldMapTree = undefined

-- EXERCISE: Find all that satisfy a "predicate"
-- Recursively find all that return a Just
-- ifJust might be useful here
findAll :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
findAll = undefined
