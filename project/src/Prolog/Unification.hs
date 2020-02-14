module Prolog.Unification
  ( -- Replacement
  , Substitution
  , unify
  , subVar
  , subTerm
  , subAtom
  ) where

-- An equation of two terms, represents the goal of making them equal.
data Matching = Term :==: Term
  deriving (Show, Eq)

instance Functor Matching where
  fmap f (t :==: k) = f t :==: f k

-- A variable name to be replaced with a term
data Replacement = UpperName := Term
-- List of replacements, whose variable names should not intersect.
type Substitution = [Replacement]

subVar :: Substitution -> UpperName -> Term
subVar subst x = case mapMaybe sameVarTerm of
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

subMatchings :: Substition -> [Matching] -> [Matching]
subMatchings subst = map $ fmap $ subTerm subst


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
match _ _ = Nothing
