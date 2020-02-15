module Prolog.Unification
  ( Substitution
  , subVar
  , subTerm
  , subAtom
  , composeSub
  , vars
  , unify
  , unifyAtoms
  ) where

import Prolog.Program
  ( UpperName
  , Term(..)
  , Atom(..)
  )

import Data.List.NonEmpty (toList)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Control.Monad (guard)

-- An equation of two terms, represents the goal of making them equal.
data Matching = Term :==: Term
  deriving (Show, Eq)

mapTerms :: (Term -> Term) -> Matching -> Matching
mapTerms f (t :==: k) = f t :==: f k

-- A variable name to be replaced with a term
data Replacement = UpperName := Term
  deriving (Eq)
-- List of replacements, whose variable names should not intersect.
type Substitution = [Replacement]

instance Show Replacement where
  show (x := t) = x ++ " := " ++ show t

subVar :: Substitution -> UpperName -> Term
subVar subst x = case mapMaybe sameVarTerm subst of
  []    -> Var x
  (t:_) -> t
  where sameVarTerm :: Replacement -> Maybe Term
        sameVarTerm (y := t)
          | x == y    = Just t
          | otherwise = Nothing
          

subTerm :: Substitution -> Term -> Term
subTerm subst (Var x)     = subVar subst x
subTerm subst (Func f ts) = Func f $ map (subTerm subst) ts

subAtom :: Substitution -> Atom -> Atom
subAtom subst (Atom a ts) = Atom a $ fmap (subTerm subst) ts

subMatchings :: Substitution -> [Matching] -> [Matching]
subMatchings subst = map $ mapTerms $ subTerm subst

subReplacement :: Substitution -> Replacement -> Replacement
subReplacement subst (x := t) = x := subTerm subst t


-- Composes two substitutions. Contains duplicates if s1 and s2 intersect
composeSub :: Substitution -> Substitution -> Substitution
composeSub s1 s2
  =  map (subReplacement s2) s1
  ++ filter (not . (`elem` vars s1) . getVar) s2

getVar :: Replacement -> UpperName
getVar (x := _) = x

vars :: Substitution -> [UpperName]
vars = map getVar

unifyAtoms :: Atom -> Atom -> Maybe Substitution
unifyAtoms (Atom x ts) (Atom y ks) = do
  guard $ x == y
  unify $ (zipWith (:==:) `on` toList) ts ks

-- Makes a substitution, i.e. a mapping Var -> Term,
-- from a list of matchings, i.e. a list of wanted equations of terms t :==: k.
-- Returns Nothing if no such substitution exists.
unify :: [Matching] -> Maybe Substitution
unify [] = Just []
unify ((t :==: k) : ms)
  | t == k    = unify ms
  | otherwise = match t k ms

match :: Term -> Term -> [Matching] -> Maybe Substitution
match (Func f ts) (Func g ks) ms
  | f == g && length ts == length ks
    = unify $ ms' ++ ms
  | otherwise = Nothing
  where ms' = zipWith (:==:) ts ks
match (Var x) t ms
  = (subst ++) <$> unify (subMatchings subst ms)
  where subst = [x := t]
match t v@(Var _) ms
  = match v t ms
