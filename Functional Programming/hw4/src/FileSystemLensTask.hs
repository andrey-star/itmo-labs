{-# LANGUAGE OverloadedStrings #-}

-- | This module contains lenses and prisms for a file system data type.
module FileSystemLensTask 
  ( FS (..)
  , scanFS
  , name
  , contents
  , _File
  , _Dir
  ) where

import Lens.Micro (Lens', Traversal', lens)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (splitPath, takeFileName, (</>))

-- | Represents a file system with directories and files.
data FS
  = Dir
      { _name     :: FilePath,  -- Represents the name of the directory.
        _contents :: [FS]       -- Represents the contents of the directory
      }
  | File
      { _name :: FilePath       -- Represents the name of the file.
      }

-- | Scans the file system from the given root directory.
scanFS :: FilePath -> IO FS
scanFS path = do
  fileExists <- doesFileExist path
  if fileExists
    then return (File (takeFileName path))
    else do
      dirContents <- listDirectory path
      subFss <- mapM (scanFS . (path </>)) dirContents
      return (Dir (last (splitPath path)) subFss)

-- | '_name' field lens for 'FS' data type.
name :: Lens' FS FilePath
name = lens _name (\fs n -> fs {_name = n})

-- | '_contents' field lens for 'FS' data type.
contents :: Traversal' FS [FS]
contents f dir@(Dir _ _) = lens _contents (\d c' -> d {_contents = c'}) f dir
contents _ file          = pure file

-- | 'File' prism for 'FS' data type.
_File :: Traversal' FS FS
_File f file@(File _) = f file
_File _ dir           = pure dir

-- | 'Dir' prism for 'FS' data type.
_Dir :: Traversal' FS FS
_Dir f dir@(Dir _ _) = f dir
_Dir _ file          = pure file
