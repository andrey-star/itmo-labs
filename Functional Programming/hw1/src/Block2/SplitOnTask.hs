-- | Contains functions for splitting/joining lists with a separator.
module Block2.SplitOnTask
  ( splitOn
  , joinWith
  ) where

import Data.List.NonEmpty (NonEmpty (..), (<|))

-- | Returns the result of splitting the values of the provided list
-- with the given separator.
-- e.g. splitOn '/' "path/to/file" produces ["path", "to", "file"]
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr (fold sep) ([] :| [])
  where
    fold :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
    fold separator cur res@(x :| xs)
      | cur == separator = [] <| res
      | otherwise = (cur : x) :| xs

-- | Returns the result of joining the values of the provided list
-- with the given seprator.
-- e.g. joinWith '/' ["path", "to", "file"] produces "path/to/file"
joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep = foldl1 (\a b -> a ++ (sep : b))
