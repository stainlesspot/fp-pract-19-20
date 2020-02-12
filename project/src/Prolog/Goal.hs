module Prolog.Goal where

import Prolog.DataTypes
import Data.List.NonEmpty (NonEmpty(..), toList)

type Goal = [Atom]

data Restriction = Term :=: Term
  deriving (Show, Eq)

type Subst = [(UName,Term)]

resolveStep :: Goal -> HornClause -> Maybe (Goal, Subst)
resolveStep []     _         = Just ([], []) -- the empty goal is true
resolveStep (a:as) (r :- ps) = do
  sub <- unify $ equify r a 
  return (ps ++ as, sub)

provides :: Atom -> HornClause -> Bool
provides a'@(Atom a _) (b'@(Atom b _) :- _)
  =  a == b
  && toBool (unify $ equify a' b')
    where toBool Nothing  = False
          toBool (Just _) = True

first :: [Maybe a] -> Maybe a
first [] = Nothing
first ((Just x):_) = Just x
first (Nothing : mxs) = first mxs

justs :: [Maybe a] -> [a]
justs [] = []
justs (Nothing  : xs) = justs xs
justs ((Just x) : xs) = x : justs xs


resolve :: Program -> Goal -> Maybe Subst
resolve _ [] = Just []
resolve p g@(a:_)
  = first
  $ map continue 
  $ justs
  $ map (resolveStep g)
  $ filter (provides a) p
    where continue :: (Goal, Subst) -> Maybe Subst
          continue (g', substs) = do
            substs' <- resolve p $ subGoal substs g'
            return $ substs ++ substs'


        

equify :: Atom -> Atom -> [Restriction]
equify (Atom _ ts) (Atom _ ks) = zipWith (:=:) (toList ts) (toList ks)

--toSubst :: [Restriction] 

unify :: [Restriction] -> Maybe Subst
unify [] = Just []
unify ((t :=: k) : rs)
  | t == k = unify rs
  | otherwise = match t k
  where match (Func f ts) (Func g ks) =
          if f == g && length ts == length ks
            then unify $ zipWith (:=:) (toList ts) (toList ks) ++ rs
            else Nothing
        match (Const _) (Const _) = Nothing -- because t != k
        match t' v@(Var _) = unify $ (v :=: t') : rs
        match (Var v) t' = (s++) <$> unify (substituteR s rs)
          where s = [(v,t')]
        match _ _ = Nothing


substituteR :: Subst -> [Restriction] -> [Restriction]
substituteR ss = map sub
  where sub (t :=: k) = substitute ss t :=: substitute ss k

subGoal :: Subst -> Goal -> Goal
subGoal ss = map $ substituteA ss

substituteA :: Subst -> Atom -> Atom
substituteA ss (Atom a ts) = Atom a (substitute ss <$> ts)

substitute :: Subst -> Term -> Term
substitute ss (Var u) = case lookup u ss of
  (Just t) -> t
  _        -> Var u
substitute _ c@(Const _) = c
substitute ss (Func f ks) = Func f $ fmap (substitute ss) ks
