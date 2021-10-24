module HW1.T7
  ( ListPlus(..)
  , Inclusive(..)
  , DotString(..)
  , Fun(..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a
infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last last')      = (:+) last'
  (<>) (last' :+ front') = (:+) last' . (<>) front'

data Inclusive a b = This a | That b | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This x)   (This x')    = This $ x <> x'
  (<>) (This x)   (That y)     = Both x y
  (<>) (This x)   (Both x' y)  = Both (x <> x') y
  (<>) (That y)   (This x)     = Both x y
  (<>) (That y)   (That y')    = That $ y <> y'
  (<>) (That y)   (Both x y')  = Both x (y <> y')
  (<>) (Both x y) (This x')    = Both (x <> x') y
  (<>) (Both x y) (That y')    = Both x (y <> y')
  (<>) (Both x y) (Both x' y') = Both (x <> x') (y <> y')

newtype DotString = DS String

instance Semigroup DotString where
  (<>) x       (DS "") = x
  (<>) (DS "") y       = y
  (<>) (DS a)  (DS b)  = DS $ concat [a, ".", b]

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  (<>) (F f) (F g) = F $ g . f

instance Monoid (Fun a) where
  mempty = F id
