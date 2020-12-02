{-# LANGUAGE InstanceSigs #-}

-- | Contains binary search tree type and related methods.
module Block1.TreeTask
  ( Tree (..)
  , empty
  , size
  , contains
  , add
  , fromList
  , remove
  ) where

import Data.List.NonEmpty (NonEmpty (..), (<|))

-- | Binary search representation. Each non empty
-- node stores a list of equal values.
data Tree a
  = Leaf                                 -- ^ Represents an empty node with no subtrees
  | Node (NonEmpty a) (Tree a) (Tree a)  -- ^ Represents a non empty node
  deriving (Show)

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) Leaf Leaf = True
  (==) (Node v1 l1 r1) (Node v2 l2 r2) = (v1 == v2) && (l1 == l2) && (r1 == r2)
  (==) _ _ = False

-- | Returns true, if the specified 'Tree' is empty, false otherwise
empty :: Tree a -> Bool
empty Leaf = True
empty _ = False

-- | Returns the size of the specified 'Tree'.
-- The size is the total number of values in each non empty node.
size :: Tree a -> Int
size Leaf = 0
size (Node vals l r) = size l + size r + length vals

-- | Returns true if the specified key contains the provided key,
-- false otherwise
contains :: Ord a => Tree a -> a -> Bool
contains Leaf _ = False
contains (Node (v :| _) l r) k
  | k > v = contains r k
  | k < v = contains l k
  | otherwise = True

-- | Returns a 'Tree' with the given value added to the provided 'Tree'
add :: Ord a => Tree a -> a -> Tree a
add Leaf k = Node (k :| []) Leaf Leaf
add (Node vals@(v :| _) l r) k
  | k > v = Node vals l (add r k)
  | k < v = Node vals (add l k) r
  | otherwise = Node (k <| vals) l r

-- | Returns a 'Tree' constructed form the provided 'List'
fromList :: Ord a => [a] -> Tree a
fromList = foldl add Leaf

-- | Returns a 'Tree' with the given value removed from the provided 'Tree'
remove :: Ord a => Tree a -> a -> Tree a
remove Leaf _ = Leaf
remove (Node vals@(v :| vs) l r) k
  | k > v = Node vals l (remove r k)
  | k < v = Node vals (remove l k) r
  | otherwise = case vs of
    h : t -> Node (h :| t) l r
    [] -> case r of
      Leaf -> l
      _ -> case removeLeftmost r of
        Nothing -> error "assertion error"
        Just (vv, rr) -> Node vv l rr

removeLeftmost :: Tree a -> Maybe (NonEmpty a, Tree a)
removeLeftmost Leaf = Nothing
removeLeftmost (Node v l r) =
  case removeLeftmost l of
    Nothing -> Just (v, r)
    Just (lv, ll) -> Just (lv, Node v ll r)
    
instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Node vals l r) = foldMap f l `mappend` foldMap f vals `mappend` foldMap f r

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf = z
  foldr f z (Node vals l r) = foldr f (foldr f (foldr f z r) vals) l
  