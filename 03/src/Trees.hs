{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=HLint #-} -- run hlint on build via the hlint source plugin

module Trees where

import Prelude
import Data.Monoid (Sum(..), All(..), Any(..), First(..))
import Data.Maybe (isJust)
import Data.List (foldl')

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving Show

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  Empty == Empty = True
  Node x1 l1 r1 == Node x2 l2 r2
    =  x1 == x2
    && l1 == l2
    && r1 == r2
  _ == _ = False

leaf :: a -> Tree a
leaf x = Node x Empty Empty

insertOrdered :: Ord a => a -> Tree a -> Tree a
insertOrdered x Empty        = leaf x
insertOrdered x (Node y l r) =
  if x <= y
  then Node y (insertOrdered x l) r
  else Node y l (insertOrdered x r)

listToBST :: Ord a => [a] -> Tree a
listToBST = foldl' (flip insertOrdered) Empty

isBST :: Ord a => Tree a -> Bool
-- first idea
isBST Empty = True
isBST (Node x l r) = check (<=) l && check (>) r
  where check _    Empty          = True
        check (~~) t@(Node y _ _) = y ~~ x && isBST t
-- also works
--isBST = between Bot Top

data BotTop a = Bot | Val a | Top
  deriving (Show, Eq, Ord)

between :: Ord a => BotTop a -> BotTop a -> Tree a -> Bool
between low high Empty        = low <= high
between low high (Node x l r)
  =  low < vx
  && vx <= high
  && between low vx   l
  && between vx  high r
  where vx = Val x

findBST :: Ord a => a -> Tree a -> Bool
findBST _ Empty        = False
findBST x (Node y l r) = case compare x y of
  EQ -> True
  LT -> findBST x l
  GT -> findBST x r

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty        = Empty
mapTree f (Node x l r) = Node (f x) (mapTree f l) (mapTree f r)

foldTree :: Monoid a => Tree a -> a
foldTree Empty        = mempty
foldTree (Node x l r) = foldTree l <> x <> foldTree r

foldMapTree :: Monoid b => (a -> b) -> Tree a -> b
foldMapTree f = foldTree . mapTree f

sumTree :: Num a => Tree a -> a
sumTree = getSum . foldMapTree Sum

allTree :: (a -> Bool) -> Tree a -> Bool
allTree f = getAll . foldMapTree (All . f)

treeToList :: Tree a -> [a]
treeToList = foldMapTree return
-- if return is forbidden
--treeToList = foldMapTree (:[])

elemTree :: Eq a => a -> Tree a -> Bool
elemTree x = getAny . foldMapTree (Any . (== x))

onMaybe :: (a -> Bool) -> a -> Maybe a
onMaybe p x = if p x then Just x else Nothing

findPred :: (a -> Bool) -> Tree a -> Maybe a
findPred p = getFirst . foldMapTree (First . onMaybe p)

findAll :: (a -> Bool) -> Tree a -> [a]
findAll p = foldMapTree (filter p . return)
-- if return is forbidden
--findAll p = foldMapTree (filter p . (:[]))
 
-- same as (>>=) for the Maybe monad
ifJust :: Maybe a -> (a -> Maybe b) -> Maybe b
ifJust Nothing  _ = Nothing
ifJust (Just x) f = f x

validateTree :: (a -> Maybe b) -> Tree a -> Maybe (Tree b)
validateTree _ Empty = Just Empty
-- coolest
validateTree f (Node x l r) = do
  l' <- validateTree f l
  y  <- f x
  r' <- validateTree f r
  Just $ Node y l' r'

-- first idea
--validateTree f (Node x l r)
--  = validateTree f l >>= \l' ->
--    f x >>= \y ->
--    validateTree f r >>= \r' ->
--    Just $ Node y l' r'

-- if (>>=) and do are forbidden
--validateTree f (Node x l r)
--  = validateTree f l `ifJust` \l' ->
--    f x `ifJust` \y ->
--    validateTree f r `ifJust` \r' ->
--    Just $ Node y l' r'

data Direction
  = L -- go left
  | R -- go right
  deriving (Show, Eq)

fetch :: [Direction] -> Tree a -> Maybe a
fetch []     (Node x _ _) = Just x
fetch (d:ds) (Node _ l r)
  = fetch ds (case d of L -> l
                        R -> r)
fetch _ _ = Nothing

paths :: Tree a -> [(a, [Direction])]
paths = treeToList . mapDirections

-- also in Control.Arrow
second :: (a -> b) -> (c,a) -> (c,b)
second f (x,y) = (x, f y)

mapDirections :: Tree a -> Tree (a, [Direction])
mapDirections = mapTree (second reverse) . go []
  where go _    Empty        = Empty
        go path (Node x l r) = Node (x, path) (go (L:path) l) (go (R:path) r)
