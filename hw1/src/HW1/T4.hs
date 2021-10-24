module HW1.T4
  ( tfoldr
  , treeToList
  ) where

import           HW1.T3 (Tree (..))

-- | Analogue for foldr operation for custom Tree.
tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ i Leaf             = i
tfoldr f i (Branch _ l v r) = tfoldr f (f v $ tfoldr f i r) l

-- | Conversion to List for custom Tree.
treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []
