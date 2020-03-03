module Prolog.Unification
  ( Substitution
  , Replacement(..)
  , emptySubst
  , subVar
  , subTerm
  , subAtom
  , composeSub
  , getVar
  , vars
  , unify
  , unifyAtoms
  ) where

import Prolog.Program (Term(..), Atom(..))

import Data.List.NonEmpty (toList)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Control.Monad (guard)

-- A variable name to be replaced with a term.
data Replacement = String := Term
  deriving (Show, Eq)
-- List of replacements, whose variable names should not intersect.
type Substitution = [Replacement]

-- An equation of two terms,
-- represents the need to make them equal through a substitution.
data Matching = Matching Term Term
  deriving (Show, Eq)

mapTerms :: (Term -> Term) -> Matching -> Matching
mapTerms f (Matching t k) = Matching (f t) (f k)

emptySubst :: Substitution
emptySubst = []

-- Substitute a variable name with its corresponding term.
subVar :: Substitution -> String -> Term
subVar subst x = case mapMaybe sameVarTerm subst of
  []    -> Var x
  (t:_) -> t
  where
    sameVarTerm :: Replacement -> Maybe Term
    sameVarTerm (y := t)
      | x == y    = Just t
      | otherwise = Nothing
          
-- Substitute all vars in a term.
subTerm :: Substitution -> Term -> Term
subTerm subst (Var x)     = subVar subst x
subTerm subst (Func f ts) = Func f $ map (subTerm subst) ts

-- Substitute all vars in an atom.
subAtom :: Substitution -> Atom -> Atom
subAtom subst (Atom a ts) = Atom a $ fmap (subTerm subst) ts

-- Substitute all vars in a matching.
subMatchings :: Substitution -> [Matching] -> [Matching]
subMatchings subst = map $ mapTerms $ subTerm subst

-- Substitute all vars in the term of a replacement.
subReplacement :: Substitution -> Replacement -> Replacement
subReplacement subst (x := t) = x := subTerm subst t


-- Composes two substitutions.
composeSub :: Substitution -> Substitution -> Substitution
composeSub s1 s2
  =  map (subReplacement s2) s1
  ++ filter (not . (`elem` vars s1) . getVar) s2

getVar :: Replacement -> String
getVar (x := _) = x

vars :: Substitution -> [String]
vars = map getVar

unifyAtoms :: Atom -> Atom -> Maybe Substitution
unifyAtoms (Atom x ts) (Atom y ks) = do
  guard $ x == y && length ts == length ks
  unify $ (zipWith Matching `on` toList) ts ks

-- Makes a substitution, i.e., a mapping Var -> Term,
-- from a list of matchings, i.e., a list of wanted equations of terms t == k.
-- Returns Nothing if no such substitution exists.
unify :: [Matching] -> Maybe Substitution
unify [] = Just []
unify (Matching t k : ms)
  | t == k    = unify ms
  | otherwise = match t k ms

match :: Term -> Term -> [Matching] -> Maybe Substitution
match (Func f ts) (Func g ks) ms
  | f == g && length ts == length ks
    = unify $ ms' ++ ms
  | otherwise = Nothing
  where
    ms' = zipWith Matching ts ks

match (Var x) t ms = (subst ++) <$> unify (subMatchings subst ms)
  where
    subst = [x := t]

match t v@(Var _) ms = match v t ms
