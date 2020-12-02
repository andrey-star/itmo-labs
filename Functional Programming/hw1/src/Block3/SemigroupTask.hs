{-# LANGUAGE InstanceSigs #-}

-- | Contains various 'Semigroup' and 'Monoid' type instances.
module Block3.SemigroupTask
  ( NonEmpty (..)
  , ThisOrThat (..)
  , Name (..)
  , Endo (..)
  ) where

-- | Non empty list representation
data NonEmpty a = a :| [a]
  deriving (Show)

instance Eq a => Eq (NonEmpty a) where
  (==) :: NonEmpty a -> NonEmpty a -> Bool
  (==) (x :| xs) (y :| ys) = x == y && xs == ys

instance Semigroup (NonEmpty a) where
  (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
  (<>) (x :| xs) (y :| ys) = x :| (xs ++ y : ys)

-- | At least on of two elements representation.
data ThisOrThat a b
  = This a    -- ^ Represents first value present
  | That b    -- ^ Represents second value present
  | Both a b  -- ^ Represents two values present
  deriving (Show)

instance (Eq a, Eq b) => Eq (ThisOrThat a b) where
  (==) :: ThisOrThat a b -> ThisOrThat a b -> Bool
  (==) (This a) (This b)         = a == b
  (==) (That a) (That b)         = a == b
  (==) (Both a1 b1) (Both a2 b2) = a1 == a2 && b1 == b2
  (==) _ _                       = False

instance Semigroup (ThisOrThat a b) where
  (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
  (<>) (This a) (This _)     = This a
  (<>) (That a) (That _)     = That a
  (<>) (This a) (That b)     = Both a b
  (<>) (That b) (This a)     = Both a b
  (<>) (Both a b) (This _)   = Both a b
  (<>) (Both a _) (That b)   = Both a b
  (<>) (This a) (Both _ b)   = Both a b
  (<>) (That _) (Both a b)   = Both a b
  (<>) (Both a _) (Both _ b) = Both a b

-- | Wrapper for 'String'
newtype Name
  = Name String
  deriving (Show)

instance Eq Name where
  (==) :: Name -> Name -> Bool
  (==) (Name a) (Name b) = a == b

instance Semigroup Name where
  (<>) :: Name -> Name -> Name
  (<>) (Name a) (Name "") = Name a
  (<>) (Name "") (Name b) = Name b
  (<>) (Name a) (Name b)  = Name (a ++ "." ++ b)

instance Monoid Name where
  mempty :: Name
  mempty = Name ""

-- | Wrapper for a function accepting and returning a value of same type.
newtype Endo a = Endo
  { getEndo :: a -> a
  }

instance Semigroup (Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (<>) Endo {getEndo = f} Endo {getEndo = g} = Endo (f . g)

instance Monoid (Endo a) where
  mempty :: Endo a
  mempty = Endo {getEndo = id}
