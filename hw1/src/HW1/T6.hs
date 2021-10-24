module HW1.T6
  ( mcat
  , epart
  ) where

import           Data.Bifunctor (bimap)
import           Data.Either    (fromLeft, fromRight)
import           Data.Foldable  (foldl', foldr')

-- | Mappending all existing values from List.
mcat :: Monoid a => [Maybe a] -> a
mcat = foldl' (foldl' mappend) mempty

-- | Mappending all existing values from Left and Right values to Tuple.
epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldr' bimapTupleEither (mempty, mempty)
  where
    bimapTupleEither y = bimap (mappend $ fromLeft mempty y) (mappend $ fromRight mempty y)
