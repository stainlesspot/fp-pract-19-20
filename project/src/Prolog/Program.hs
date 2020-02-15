module Prolog.Program
  ( UpperName
  , LowerName
  , Term(..)
  , Atom(..)
  , HornClause(..)
  , Program
  ) where

import Data.List.NonEmpty (NonEmpty(..), toList)

-- Represents a string, starting with an upper-case letter.
type UpperName = String
-- Represents a string, starting with a lower-case letter.
type LowerName = String
-- All names (UpperName and LowerName) are alpha-numeric
-- and may contain underscores.

-- A term is a variable or a function symbol with subterms.
-- A function symbol with no subterms, i.e. arity 0, is called a constant.
--
-- Variables start with an upper-case letter
-- and function symbols start with a lower-case letter.
data Term = Var UpperName | Func LowerName [Term]
  deriving (Eq)

showTuple :: Show a => NonEmpty a -> String
showTuple (x:|[]) = show x
showTuple (x:|(y:xs)) = show x ++ ", " ++ showTuple (y:|xs)

instance Show Term where
  show (Var x) = x
  show (Func f []) = f
  show (Func f (t:ts)) = f ++ "(" ++ showTuple (t:|ts) ++ ")"

-- Atoms start with a lower-case letter and have at least one term
data Atom = Atom LowerName (NonEmpty Term) 
  deriving (Show, Eq)

-- HornClause with empty body is called a fact.
-- HornClause with nonempty body is called a rule.
data HornClause = Atom :- [Atom]
  deriving (Show, Eq)

type Program = [HornClause]
