module StuffNonEmpty
  ( NonEmpty(..)
  , mapNonEmpty
  , groupNonEmpty
  , groupByNonEmpty
  , groupOnNonEmpty
  , classifyOnNonEmpty
  ) where

import Stuff (sortOn, sortBy, on, (&&&))

groupNonEmpty :: Eq a => [a] -> [NonEmpty a]
groupNonEmpty = groupByNonEmpty (==)

data NonEmpty a = a :| [a]
  deriving (Show, Eq, Ord)
infixr 4 :|

mapNonEmpty :: (a -> b) -> NonEmpty a -> NonEmpty b
mapNonEmpty f (x:|xs) = f x :| map f xs

groupByNonEmpty :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupByNonEmpty (~~) = foldr insertGroup []
  where insertGroup x [] = [x:|[]]
        insertGroup x rec@((y:|ys):zss) =
          if x ~~ y
          then (x:|y:ys):zss
          else (x:|[]):rec

groupOnNonEmpty :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
groupOnNonEmpty f
  = map (mapNonEmpty fst)
  . groupByNonEmpty ((==) `on` snd)
  . map (id &&& f)

classifyByNonEmpty :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
classifyByNonEmpty (~~) = foldr insertClass []
  where insertClass x [] = [x:|[]]
        insertClass x (ys@(y:|ys'):zss) =
          if x ~~ y
          then (x:|y:ys'):zss
          else ys : insertClass x zss

classifyOnNonEmpty :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
classifyOnNonEmpty f
  = map (mapNonEmpty fst)
  . classifyByNonEmpty ((==) `on` snd)
  . map (id &&& f)
