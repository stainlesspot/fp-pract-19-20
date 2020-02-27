module Prolog.Program
  ( UpperName
  , LowerName
  , Term(..)
  , Atom(..)
  , HornClause(..)
  , Program
  , mapAtoms
  , mapTerms
  , isUpperNameStart
  ) where

import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Char (isUpper)

-- Represents a string, starting with an upper-case letter.
type UpperName = String
-- Represents a string, starting with a lower-case letter.
type LowerName = String
-- All names (UpperName and LowerName) are alpha-numeric
-- and may contain underscores.

isUpperNameStart :: Char -> Bool
isUpperNameStart = isUpper

-- A term is a variable or a function symbol with subterms.
-- A function symbol with no subterms, i.e. arity 0, is called a constant.
--
-- User variables start with an upper-case letter,
-- but other variable names may also be generated.
-- Function symbols start with a lower-case letter.
data Term = Var String | Func LowerName [Term]
  deriving (Show, Eq)

-- Atoms start with a lower-case letter and have at least one term
data Atom = Atom LowerName (NonEmpty Term) 
  deriving (Show, Eq)

-- HornClause with empty body is called a fact.
-- HornClause with nonempty body is called a rule.
data HornClause = Atom :- [Atom]
  deriving (Show, Eq)

type Program = [HornClause]

mapAtoms :: (Atom -> Atom) -> Program -> Program
mapAtoms f = map changeClause
  where changeClause (a :- as) = f a :- map f as

mapTerms :: (Term -> Term) -> Program -> Program
mapTerms f = mapAtoms changeAtom
  where changeAtom (Atom a ts) = Atom a $ fmap f ts
