{-# LANGUAGE InstanceSigs #-}

-- | Contains a 2D grid implementation with ListZipper.
module Grid
  ( Grid (..)
  , left
  , right
  , up
  , down
  , gridRead
  , gridWrite
  , ListZipper (..)
  , listLeft
  , listRight
  , toList
  ) where

import Control.Comonad (Comonad, duplicate, extract)

-- | Represents a ListZipper, which focuses on a specific element
-- in a collection.
data ListZipper a = LZ [a] a [a]

-- | Move left or right in a 'ListZipper'.
listLeft, listRight :: ListZipper a -> ListZipper a
listLeft (LZ (a : as) x bs) = LZ as a (x : bs)
listLeft _                  = error "listLeft"
listRight (LZ as x (b : bs)) = LZ (x : as) b bs
listRight _                  = error "listRight"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

-- | Convert 'ListZipper' to regular list
-- with n elements on each side.
toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

instance Functor ListZipper where
  fmap :: (a -> b) -> ListZipper a -> ListZipper b
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

genericMove ::
  (z a -> z a) ->
  (z a -> z a) ->
  z a ->
  ListZipper (z a)
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)

instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract (LZ _ x _) = x

  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate = genericMove listLeft listRight

-- | Represents a 2D grid.
newtype Grid a = Grid {unGrid :: ListZipper (ListZipper a)}

-- | Move up or down in a 'Grid'.
up, down :: Grid a -> Grid a
up (Grid g) = Grid (listLeft g)
down (Grid g) = Grid (listRight g)

-- | Move left or right in a 'Grid'.
left, right :: Grid a -> Grid a
left (Grid g) = Grid (fmap listLeft g)
right (Grid g) = Grid (fmap listRight g)

-- | Read a value from focused element in 'Grid'.
gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

-- | Write a value to focused element in 'Grid'.
gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right
vertical = genericMove up down

instance Functor Grid where
  fmap f (Grid grid) = Grid (fmap (fmap f) grid)

instance Comonad Grid where
  extract :: Grid a -> a
  extract = gridRead

  duplicate :: Grid a -> Grid (Grid a)
  duplicate = Grid . fmap horizontal . vertical
