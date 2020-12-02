{-# LANGUAGE InstanceSigs #-}

-- | Contains instances for Tree data structure
module Block1.TreeInstanceTask 
  ( Tree (..)
  ) where

-- | Tree representation. Data is stored in leaves.
data Tree a
  = Branch (Tree a) (Tree a)  -- ^ Represents an inner tree node with two branches
  | Leaf a deriving Show      -- ^ Represents a leaf node with stored data

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (==) (Leaf a) (Leaf b)             = a == b
  (==) (Branch l1 r1) (Branch l2 r2) = l1 == l2 && r1 == r2
  (==) _ _                           = False

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf e)     = Leaf (f e)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
  pure :: a -> Tree a
  pure e = Leaf e

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) (Leaf f) t     = fmap f t
  (<*>) (Branch l r) t = Branch (l <*> t) (r <*> t)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z (Leaf e)     = f e z
  foldr f z (Branch l r) = foldr f (foldr f z r) l

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f (Leaf e)     = f e
  foldMap f (Branch l r) = foldMap f l <> foldMap f r

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf e)     = Leaf <$> f e
  traverse f (Branch l r) = Branch <$> traverse f l <*> traverse f r
