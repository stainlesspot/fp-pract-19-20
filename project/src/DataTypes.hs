module DataTypes
  ( NonEmpty(..)
  , Term(..)
  , Atom(..)
  , HornClause(..)
  , UName
  , LName
  , Program
  ) where

import Data.List.NonEmpty (NonEmpty(..))

-- Represents string, starting with upper-case letter.
type UName = String
-- Represents string, starting with lower-case letter.
type LName = String

-- All names (UName and LName) are alpha-numeric and may contain underscores.

-- Variable names start with upper-case letters.
-- Constant, Function Symbol and Atom names start with lower-case letters.

data Term = Var UName | Const LName | Func LName (NonEmpty Term)
  deriving (Show, Eq)

data Atom = Atom LName (NonEmpty Term) 
  deriving (Show, Eq)

-- Facts have no premises,
-- so for example the fact "nat(z)." is repesented by
-- Atom "nat" (Const "z":|[]) :- []
--
-- Rules have at least one premise.
--
-- HornClause also repesents queries.
data HornClause = Atom :- [Atom]
  deriving (Show, Eq)

type Program = [HornClause]

-- type Query
