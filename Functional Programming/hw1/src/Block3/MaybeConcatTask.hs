-- | Contains functions for concatenating 'Monoid's.
module Block3.MaybeConcatTask
  ( maybeConcat
  , eitherConcat
  ) where

-- | Returns the result of concatenating the lists
-- wrapped in 'Just' context, ignoring 'Nothing'.
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldl fold []
  where
    fold :: [a] -> Maybe [a] -> [a]
    fold l Nothing    = l
    fold l1 (Just l2) = l1 <> l2

-- | Returns a par containing the result of folding the provided
-- 'Left's and 'Right's separately.
eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = foldl fold (mempty, mempty)
  where
    fold :: (Monoid a, Monoid b) => (a, b) -> Either a b -> (a, b)
    fold (a, b) (Left l)  = (a <> l, b)
    fold (a, b) (Right r) = (a, b <> r)
