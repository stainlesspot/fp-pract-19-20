module Stuff
  ( group
  , sortBy
  , groupBy
  , sortOn
  , groupOn
  , classifyOn
  , (&&&)
  , on
  ) where

import Data.List (partition)

group :: Eq a => [a] -> [[a]]
group = groupBy (==)

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _   x []     = [x] 
insertBy cmp x (y:ys) = case cmp x y of
  GT -> y : insertBy cmp x ys
  _  -> x:y:ys

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = foldr (insertBy cmp) []

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy (~~) = foldr insertGroup []
  where insertGroup x [] = [[x]]
        insertGroup _ ([]:_) = error "should never happen"
        insertGroup x rec@(ys@(y:_):zss) =
          if x ~~ y
          then (x:ys):zss
          else [x]:rec

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(g `on` f) x y = g (f x) (f y)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(f &&& g) x = (f x, g x)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f
  = map fst
  . sortBy (compare `on` snd)
  . map (id &&& f)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f
  = map (map fst)
  . groupBy ((==) `on` snd)
  . map (id &&& f)

classifyBy :: (a -> a -> Bool) -> [a] -> [[a]]
classifyBy _    []     = []
classifyBy (~~) (x:xs) = (x:ts) : classifyBy (~~) fs
  where (ts,fs) = partition (~~ x) xs

-- Ако искаш да имплементирам сам partition
--partition :: (a -> Bool) -> [a] -> ([a],[a])
--partition p = foldr place ([],[])
--  where place x (ts,fs) =
--          if p x
--          then (x:ts,fs)
--          else (ts,x:fs)

classifyOn :: Eq b => (a -> b) -> [a] -> [[a]]
classifyOn f
  = map (map fst)
  . classifyBy ((==) `on` snd)
  . map (id &&& f)
