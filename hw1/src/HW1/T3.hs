module HW1.T3
  ( Tree(..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

import Data.Foldable (foldl')

data Tree a = Leaf | Branch Int (Tree a) a (Tree a)
  deriving Show

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize Leaf             = 0
tsize (Branch n _ _ _) = n

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth Leaf             = 0
tdepth (Branch _ l _ r) = (+ 1) $ max (tdepth l) (tdepth r)

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf             = False
tmember x (Branch _ l v r) = case compare x v of
  LT -> tmember x l
  EQ -> True
  GT -> tmember x r

-- | Insert an element into the tree and strictly increment the size of the tree, O(log n)
tinsert' :: Ord a => a -> Tree a -> Tree a
tinsert' x Leaf                  = Branch 1 Leaf x Leaf
tinsert' x tree@(Branch n l v r) = case compare x v of
  LT -> Branch (n + 1) (tinsert' x l) v r
  EQ -> tree
  GT -> Branch (n + 1) l v (tinsert' x r)

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x tree = if tmember x tree then tree else tinsert' x tree

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList = foldl' (flip tinsert) Leaf
