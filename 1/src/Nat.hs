module Nat where


data Nat = Z
         | S Nat

instance Num Nat where
  Z     + y = y
  (S x) + y = S (x + y)
  x     - Z     = x
  Z     - _     = error "Subtraction of non-zero natural number from zero!"
  (S x) - (S y) = x - y
  Z     * _ = Z
  (S x) * y = y + x * y
  abs x = x
  signum _ = S Z
  fromInteger x
    | x < 0     = error "Reading a natural number from integer less than zero!"
    | otherwise = fromInteger' x
    where
      fromInteger' :: Integer -> Nat
      fromInteger' 0 = Z
      fromInteger' n = S $ fromInteger' $ n - 1

instance Eq Nat where
  Z     == Z     = True
  _     == Z     = False
  Z     == _     = False
  (S x) == (S y) = x == y

instance Ord Nat where
  compare Z     Z     = EQ
  compare _     Z     = LT
  compare Z     _     = GT
  compare (S x) (S y) = compare x y

instance Real Nat where
  toRational x = toRational $ toInteger x

instance Enum Nat where
  fromEnum x = fromEnum $ toInteger x
  toEnum x = fromInteger $ toEnum x

instance Integral Nat where
  toInteger Z     = 0
  toInteger (S x) = 1 + toInteger x
  quotRem _ Z = error "Dividing a natural number by zero!"
  quotRem x y
    | x < y     = (Z, x)
    | otherwise = let (q, r) = quotRem (x - y) y
                  in (S q, r)
  divMod = quotRem

-- the same as even :: Integral a => a -> Bool
evenNat :: Nat -> Bool
evenNat x = let (_, r) = quotRem x $ S $ S Z
            in r == Z

-- the same as gcd :: Integral a => a -> a -> a
gcdNat :: Nat -> Nat -> Nat
gcdNat x Z = x
gcdNat x y = let (_, r) = quotRem x y
             in gcdNat y r
