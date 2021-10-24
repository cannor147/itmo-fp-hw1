module HW1.T5
  ( splitOn
  , joinWith
  ) where

import           Data.Foldable      (foldr')
import           Data.List.NonEmpty (NonEmpty ((:|)))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn splitter = foldr' splitOn' $ [] :| []
  where
    splitOn' current (head' :| tail') = if splitter == current
      then (:|) [] (head' : tail')
      else (:|) (current : head') tail'

joinWith :: a -> NonEmpty [a] -> [a]
joinWith joiner (head' :| tail') = head' ++ concatMap (joiner :) tail'