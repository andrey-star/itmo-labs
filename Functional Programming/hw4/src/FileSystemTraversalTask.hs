{-# LANGUAGE Rank2Types #-}

-- | This module contains traversals a file system data type.
module FileSystemTraversalTask 
  ( cd
  , ls
  , file
  ) where

import FileSystemLensTask (FS, contents, name, _Dir, _File)
import Lens.Micro (Traversal', filtered, traversed, (^.))

-- | Emulate changing the directory to a subdir.
cd :: FilePath -> Traversal' FS FS
cd path = contents . traversed . _Dir . filtered ((== path) . (^. name))

-- | List the directory.
ls :: Traversal' FS FilePath
ls = contents . traversed . name

-- | Returns the file name, if it is present
-- in the current directory.
file :: FilePath -> Traversal' FS FilePath
file f = contents . traversed . _File . name . filtered (== f)
