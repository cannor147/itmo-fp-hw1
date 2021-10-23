module HW1.T2 
  ( N(..)
  , nplus
  , nmult
  , nsub
  , ncmp
  , nFromNatural
  , nToNum
  ) where

import GHC.Num (Natural)

data N = Z | S N
  deriving Show

nplus :: N -> N -> N
nplus Z     x = x
nplus (S x) y = S (nplus x y)

nmult :: N -> N -> N 
nmult Z         _ = Z
nmult (S Z)     x = x
nmult (S (S x)) y = nplus (nmult (S x) y) y 

nsub :: N -> N -> Maybe N
nsub x     Z     = Just x
nsub Z     _     = Nothing
nsub (S x) (S y) = nsub x y

ncmp :: N -> N -> Ordering
ncmp Z     Z     = EQ
ncmp (S _) Z     = GT
ncmp Z     (S _) = LT
ncmp (S x) (S y) = ncmp x y

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S $ nFromNatural (n - 1)

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S x) = (+ 1) $ nToNum x
