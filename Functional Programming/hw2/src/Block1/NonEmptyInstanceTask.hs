{-# LANGUAGE InstanceSigs #-}

-- | Contains instances for NonEmpty list data structure
module Block1.NonEmptyInstanceTask
  ( NonEmpty (..),
  )
where

-- | Non empty list representation.
data NonEmpty a = a :| [a] 
  deriving (Show)

instance Eq a => Eq (NonEmpty a) where
  (==) :: NonEmpty a -> NonEmpty a -> Bool
  (==) (x :| xs) (y :| ys) = x == y && xs == ys

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) (f :| fs) (x :| xs) = f x :| (map f xs ++ (fs <*> xs))

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (x :| xs) = f x (foldr f z xs)

  foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap f (x :| xs) = f x <> foldMap f xs
  
instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (x :| xs) = (:|) <$> f x <*> traverse f xs

instance Monad NonEmpty where
  return x = x :| []

  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (>>=) (x :| []) f = f x 
  (>>=) (h :| (t : ts)) f = concatNE (f h) ((t :| ts) >>= f)
    where 
    concatNE :: NonEmpty a -> NonEmpty a -> NonEmpty a
    concatNE (x :| xs) (y :| ys) = x :| (xs ++ y : ys)
   
