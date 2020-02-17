import Prelude hiding (succ, fromInteger, toInteger, pred)

type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero = const id

one :: Nat a
one = id

two :: Nat a
two f = f . f

succ :: Nat a -> Nat a
succ n f = f . (n f)

fromInteger :: Integer -> Nat a
fromInteger 0 _ = id
fromInteger z f = f . (fromInteger (z-1) f)

toInteger :: Nat Integer -> Integer
toInteger n = n (+1) 0

plus :: Nat a -> Nat a -> Nat a
plus n m f = n f . m f

mult :: Nat a -> Nat (Nat a) -> Nat a
mult n m = m (plus n) zero


type Boolean a = a -> a -> a

tt :: Boolean a
tt x y = x

ff :: Boolean a
ff x y = y

iif :: Boolean a -> a -> a -> a
iif p x y = p x y

isZero :: Nat (Boolean a) -> Boolean a
isZero n = n (const ff) tt

type Pair a = Boolean a -> a
mkPair :: a -> a -> Pair a
mkPair x y p = p x y


pred :: Nat (Pair (Nat a)) -> Nat a
pred n = n incrementPair (mkPair zero zero) tt
  where incrementPair :: Pair (Nat b) -> Pair (Nat b)
        incrementPair pair = iif (isZero mPlusOne)
                                 (mkPair m (succ mPlusOne))
                                 (mkPair (succ m) (succ mPlusOne))
          where m = pair tt
                mPlusOne = pair ff
