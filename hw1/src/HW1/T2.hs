module HW1.T2
  ( N(..)
  , ncmp
  , ndiv
  , nEven
  , nFromNatural
  , nmod
  , nmult
  , nOdd
  , nplus
  , nsub
  , nToNum
  ) where

import           GHC.Num (Natural)

-- | Custom implementation of natural numbers.
data N = Z | S N
  deriving Show

-- | Addition for custom natural numbers.
nplus :: N -> N -> N
nplus Z     x = x
nplus (S x) y = S (nplus x y)

-- | Multiplication for custom natural numbers.
nmult :: N -> N -> N
nmult Z         _ = Z
nmult (S Z)     x = x
nmult (S (S x)) y = nplus (nmult (S x) y) y

-- | Subtraction for custom natural numbers.
nsub :: N -> N -> Maybe N
nsub x     Z     = Just x
nsub Z     _     = Nothing
nsub (S x) (S y) = nsub x y

-- | Comparison for custom natural numbers.
ncmp :: N -> N -> Ordering
ncmp Z     Z     = EQ
ncmp (S _) Z     = GT
ncmp Z     (S _) = LT
ncmp (S x) (S y) = ncmp x y

-- | Conversion from Natural for custom natural numbers.
nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S $ nFromNatural (n - 1)

-- | Conversion to Num for custom natural numbers.
nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S x) = (+ 1) $ nToNum x

-- | Checking if custom natural number is even.
nEven :: N -> Bool
nEven Z     = True
nEven (S x) = not $ nEven x

-- | Checking if custom natural number is odd.
nOdd :: N -> Bool
nOdd Z     = False
nOdd (S x) = not $ nOdd x

-- | Integer division for custom natural numbers.
ndiv :: N -> N -> N
ndiv x y = case nsub x y of
  Nothing  -> Z
  (Just z) -> if ncmp z Z == EQ then S Z else S $ ndiv z y

-- | Modulo for custom natural numbers.
nmod :: N -> N -> N
nmod x y = case nsub x y of
  Nothing  -> x
  (Just z) -> if ncmp z Z == EQ then Z else nmod z y
