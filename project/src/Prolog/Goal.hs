{-# Language TupleSections #-}

module Prolog.Goal (Goal, resolve) where

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
  , emptySubst
  , subAtom
  , composeSub
  , unifyAtoms
  , getVar
  )

import Data.Maybe (mapMaybe)

-- A goal is a conjunction of atoms, which are sought to be satisfied/proven.
--data Goal = EmptyGoal | Atom :&: Goal
--infixl :&: 5
type Goal = [Atom]

-- Applies a substitution to a goal.
subGoal :: Substitution -> Goal -> Goal
--subGoal EmptyGoal = EmptyGoal
--subGoal (a :&: as)
subGoal = map . subAtom

-- Used to differentiate goal and program variables.
-- User variables always start with an upper-case letter,
-- so this prefix cannot appear in user programs or goals.
varPrefix :: String
varPrefix = "i"

-- Tries to match the atom `a` with every clause in the program.
-- Every success generates a goal and substitution.
-- The goal is the clause body and the substitution unifies `a` with the clause head.
resolveAtom :: Program -> Atom -> [(Goal, Substitution)]
resolveAtom prog a = mapMaybe matchClause prog
  where matchClause :: HornClause -> Maybe (Goal, Substitution)
        matchClause (b :- bs) = (bs,) <$> unifyAtoms a b

-- Returns all (possibly infinite) substitutions,
-- which resolve the goal from the given program.
-- Assumes no variable names intersect between the program and the goal.
resolveNI :: Program -> Goal -> [Substitution]
resolveNI _    []     = [emptySubst] -- this means the goal is trivially resolved,
                                     -- the empty list would mean the goal cannot be resolved
resolveNI prog (a:as) = concatMap (uncurry resolveRest) $ resolveAtom prog a
  where
    -- Constructs and resolves the new goal, derived by resolving `a`.
    -- The clause body and unifying substitution are given.
    -- That substitution is composed upon all results.
    resolveRest :: Goal -> Substitution -> [Substitution]
    resolveRest bs subst
      = map (composeSub subst)
      $ resolveNI prog $ subGoal subst $ bs ++ as


-- Returns all (possibly infinite) substitutions,
-- which resolve the goal from the given program.
-- To avoid clashes, program variables are renamed.
resolve :: Program -> Goal -> [Substitution]
resolve prog as
  = map (filter $ isUpperName . getVar)
  $ resolveNI (mapTerms prefixVar prog) as
  where isUpperName :: String -> Bool
        isUpperName [] = False
        isUpperName (c:_) = isUpperNameStart c

        prefixVar :: Term -> Term
        prefixVar (Var x)     = Var $ varPrefix ++ x
        prefixVar (Func f ts) = Func f $ map prefixVar ts
