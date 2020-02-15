{-# Language TupleSections #-}

module Prolog.Goal (Goal, resolve) where

import Debug.Trace

import Prolog.Program
  ( Atom
  , Term(..)
  , HornClause(..)
  , Program
  , isUpperNameStart
  , mapTerms
  )
import Prolog.Unification
  ( Substitution
  , subAtom
  , composeSub
  , unifyAtoms
  , getVar
  )

import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Maybe (mapMaybe)

type Goal = [Atom]

-- Tries to match the atom `a` with every clause in the program.
-- Every success generates a goal and substitution.
-- The goal is clause's body and the substitution unifies `a` with the clause head.
resolveAtom :: Program -> Atom -> [(Goal, Substitution)]
resolveAtom prog a = mapMaybe matchClause prog
  where matchClause :: HornClause -> Maybe (Goal, Substitution)
        matchClause (b :- bs) = (bs,) <$> unifyAtoms a b

-- Applies a substitution to a goal.
subGoal :: Substitution -> Goal -> Goal
subGoal = map . subAtom

-- resolveNonIntersecting
-- Assumes no variable names intersect between the program and the goal.
resolveNI :: Program -> Goal -> [Substitution]
resolveNI _    []     = [[]] -- list of the empty substitution
                           -- because the empty list represents failure
resolveNI prog (a:as) = concatMap (uncurry resolveRest) $ resolveAtom prog a
  where
    -- `subst` unifies `a` with some clause in `prog`,
    -- and `bs` is that clause's body.
    -- This means `a` is replaced in the original goal by `bs`.
    -- `subst` is applied on the new goal,
    -- as to reflect the unification of `a` with the clause's head.
    -- `subst` is then composed with every possible 
    resolveRest :: Goal -> Substitution -> [Substitution]
    resolveRest bs subst
      = map (composeSub subst)
      $ resolveNI prog $ subGoal subst $ bs ++ as

-- used to make program vars different from goal vars.
varPrefix :: String
varPrefix = "i"

-- Finds all substitutions, possibly infinite, for which the given goal is
-- resolved from any combination of clauses from the given program.
-- Every returned substitution, when applied to the input goal,
-- gives a conjuntion of atoms, which holds true according to the rules in the
-- given program.
resolve :: Program -> Goal -> [Substitution]
resolve prog as
  = map (filter $ isUpperName . getVar)
  $ resolveNI (traceShowId $ mapTerms prefixVar prog) as
  where isUpperName :: String -> Bool
        isUpperName [] = False
        isUpperName (c:_) = isUpperNameStart c

        prefixVar :: Term -> Term
        prefixVar (Var x)     = Var $ varPrefix ++ x
        prefixVar (Func f ts) = Func f $ map prefixVar ts
-- X = s(Y)
-- X = S(Y)
--
--Y = s(Z0)
--nat(Z0)
--Z0 = s(Z1)
--nat(Z1)
--Z1 = z
--
--
--nat(X)
--Y = s(X).
--X = s(X)
