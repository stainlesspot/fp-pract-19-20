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
insertBy _ x []     = [x] 
insertBy f x (y:ys) = if f x y == LT
                      then x:y:ys
                      else y : insertBy f x ys

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy f (x:xs) = insertBy f x $ sortBy f xs

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy f = foldr insertGroup []
  where insertGroup x [] = [[x]]
        insertGroup x ([]:zss) = [x]:zss -- should never happen
        insertGroup x rec@((y:ys):zss) =
          if f x y
          then (x:y:ys):zss
          else [x]:rec

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on g f x y = g (f x) (f y)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f g x = (f x,g x)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map fst . sortBy (compare `on` snd) . map (id &&& f)

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = map (map fst) . groupBy ((==) `on` snd) . map (id &&& f)

classifyOn :: Eq b => (a -> b) -> [a] -> [[a]]
classifyOn f = map (map fst) . classifyBy ((==) `on` snd) . map (id &&& f)

classifyBy :: (a -> a -> Bool) -> [a] -> [[a]]
classifyBy _    []     = []
classifyBy (~~) (x:xs) = (x:eqs) : classifyBy (~~) others
  where (eqs,others) = partition (~~ x) xs

-- Ако искаш да имплементирам сам partition
--partition :: (a -> Bool) -> [a] -> ([a],[a])
--partition p = foldr place ([],[])
--  where place x (ts,fs) = if p x
--                          then (x:ts,fs)
--                          else (ts,x:fs)
