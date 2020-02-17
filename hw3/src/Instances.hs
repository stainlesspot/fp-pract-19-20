{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fplugin=HLint #-} -- run hlint on build via the hlint source plugin

module Instances where

import Prelude hiding (reverse)

import Data.Char (isSpace)
import Data.Function (on)
import Control.Monad (liftM2)
import Data.List (foldl')

newtype Pointwise a b = Pointwise {getPointwise :: (a, b)}
  deriving (Show, Eq)

instance (Ord a, Ord b) => Ord (Pointwise a b) where
  (<=) :: Pointwise a b -> Pointwise a b -> Bool
  Pointwise (x1, y1) <= Pointwise (x2, y2)
    = x1 <= x2 && y1 <= y2

newtype Lexicographic a b = Lexicographic {getLexicographic :: (a, b)}
  deriving (Show, Eq)

-- The default instance for tuples and lists
instance (Ord a, Ord b) => Ord (Lexicographic a b) where
  (<=) :: Lexicographic a b -> Lexicographic a b -> Bool
  Lexicographic (x1, y1) <= Lexicographic (x2, y2)
    = x1 < x2 || (x1 == x2 && y1 <= y2)

newtype Fun a b = Fun {getFun :: a -> b}

instance (Semigroup b) => Semigroup (Fun a b) where
  (<>) :: Fun a b -> Fun a b -> Fun a b
  Fun f <> Fun g = Fun $ \x -> f x <> g x

instance (Monoid b) => Monoid (Fun a b) where
  mempty :: Fun a b
  mempty = Fun $ const mempty

newtype First a = First {getFirst :: Maybe a}
  deriving (Eq, Show)

instance Semigroup (First a) where
  (<>) :: First a -> First a -> First a
  f@(First (Just _)) <> _ = f
  _                  <> s = s

instance Monoid (First a) where
  mempty :: First a
  mempty = First Nothing

newtype Last a = Last {getLast :: Maybe a}
  deriving (Eq, Show)

instance Semigroup (Last a) where
  (<>) :: Last a -> Last a -> Last a
  _ <> l@(Last (Just _)) = l
  s <> _                 = s

instance Monoid (Last a) where
  mempty :: Last a
  mempty = Last Nothing

newtype Pair a b = Pair {getPair :: (a, b)}
  deriving (Show, Eq)


-- The default instance for tuples
instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  (<>) :: Pair a b -> Pair a b -> Pair a b
  Pair (x1, y1) <> Pair (x2, y2) = Pair (x1 <> x2, y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty :: Pair a b
  mempty = Pair (mempty, mempty)

newtype Dual a = Dual {getDual :: a}
  deriving (Show, Eq)

instance Semigroup a => Semigroup (Dual a) where
  (<>) :: Dual a -> Dual a -> Dual a
  Dual x <> Dual y = Dual $ y <> x

instance Monoid a => Monoid (Dual a) where
  mempty :: Dual a
  mempty = Dual mempty

-- Of course foldl/foldr/foldMap give different complexity.
-- foldMap is implemented as foldr, so they are the same, giving O(n^2).
-- And foldl with (flip (:)) has O(n) complexity, here I just mimic it with Dual
reverse :: [a] -> [a]
reverse = getDual . foldl' (\acc x -> acc <> Dual [x]) mempty

-- can't get faster than this
--reverse = foldl' (flip (:)) []

data Flux a = Flux
  { sides :: Maybe (a, a)
  , changes :: Int
  }
  deriving (Show, Eq)

flux :: a -> Flux a
flux x = Flux (Just (x, x)) 0

instance (Eq a) => Semigroup (Flux a) where
  (<>) :: Flux a -> Flux a -> Flux a
  Flux (Just (x1, y1)) n <> Flux (Just (x2, y2)) m
    = Flux (Just (x1, y2)) (n + m + extra)
    where extra = if y1 == x2 then 0 else 1
  f@(Flux (Just _) _) <> _ = f
  _ <> s = s
  

instance (Eq a) => Monoid (Flux a) where
  mempty :: Flux a
  mempty = Flux Nothing 0
