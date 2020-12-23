{-# LANGUAGE FlexibleInstances #-}

-- | Contains FSActions class, defining file manager functions,
--  and its instance for a real file system
module FileManager
  ( FSActions
  , RealFS
  , withCurrentDirectory
  , ls
  , createFolder
  , cat
  , createFile
  , FileManager.writeFile
  , remove
  , findFile
  , info
  , resolvePath
  , FileManagerException (..)
  ) where

import Control.Monad (filterM)
import Control.Monad.Catch (MonadCatch (..), Exception, catch, throwM)
import Control.Monad.Reader (ReaderT, ask, liftIO, runReaderT)
import Data.Time (UTCTime)
import GHC.IO.Exception (IOErrorType (InvalidArgument), ioe_type)
import System.Directory (Permissions, canonicalizePath, createDirectory,
                         doesDirectoryExist, doesFileExist, findFiles,
                         getFileSize, getModificationTime, getPermissions,
                         listDirectory, removeDirectoryRecursive, removeFile, executable, readable, writable)
import System.Directory.Internal.Prelude (isAlreadyExistsError,
                                          isDoesNotExistError,
                                          isPermissionError)
import System.FilePath.Posix (takeExtension, (</>))
import System.IO.Strict (readFile)

-- | Defines functions for a file system manager
class (MonadCatch a) => FSActions a where

  -- | Returns a list of files in the current directory
  ls :: FilePath -> a [FilePath]

  -- | Creates a directory with the given name in the current directory
  createFolder :: String -> a ()

  -- | Prints the contents of the given file
  cat :: FilePath -> a String

  -- | Creates a file with the given name in the current directory
  createFile :: String -> a ()

  -- | Writes the provided content to the given file, located in the current directory
  writeFile :: FilePath -> String -> a ()

  -- | Remove the file/folder with the given name in the current directory
  remove :: FilePath -> a ()

  -- | Searches for occurrences of the given file in the current directory and subdirectory
  findFile :: String -> a [String]

  -- | Displays information about the provided file/folder in the current directory
  info :: FilePath -> a String

-- | Represents a file system with an immutable context,
-- containing the current directory
type RealFS = ReaderT FilePath IO

-- | Represents exceptions, which might occur
-- during file manipulation
data FileManagerException
  = NoSuchPathException {errPath :: FilePath}         -- ^ Thrown, if the required path does not exist
  | PathAlreadyExistsException {errPath :: FilePath}  -- ^ Thrown, if the path already exists
  | PermissionDeniedException {errPath :: FilePath}   -- ^ Thrown, if permission is denied
  | InvalidArgumentException                          -- ^ Thrown, if an invalid argument is passed to library functions

-- | Defines error messages for FileManagerException
instance Show FileManagerException where
  show (NoSuchPathException path) = "No such file or directory: '" ++ path ++ "'"
  show (PathAlreadyExistsException path) = "File or directory already exists: '" ++ path ++ "'"
  show (PermissionDeniedException path) = "Permission denied: '" ++ path ++ "'"
  show InvalidArgumentException = "Invalid argument"

instance Exception FileManagerException

-- | Contains the implementation of the FSActions class for RealFS
instance FSActions RealFS where
  ls path =
    ( do
        curPath <- ask
        newPath <- liftIO $ resolvePath curPath path
        liftIO $ listDirectory newPath
    )
      `catch` handleError path

  createFolder name =
    ( do
        curPath <- ask
        newPath <- liftIO $ resolvePath curPath name
        liftIO $ createDirectory newPath
    )
      `catch` handleError name

  cat path =
    ( do
        curPath <- ask
        newPath <- liftIO $ resolvePath curPath path
        isDir <- liftIO $ doesDirectoryExist newPath
        if isDir then do
          throwM InvalidArgumentException
        else do
          liftIO $ System.IO.Strict.readFile newPath
    )
      `catch` handleError path

  createFile name =
    ( do
        FileManager.writeFile name ""
    )
      `catch` handleError name

  writeFile path content =
    ( do
        curPath <- ask
        newPath <- liftIO $ resolvePath curPath path
        liftIO $ Prelude.writeFile newPath content
    )
      `catch` handleError path

  remove path =
    ( do
        curPath <- ask
        newPath <- liftIO $ resolvePath curPath path
        fileExists <- liftIO $ doesFileExist newPath
        if fileExists
          then liftIO $ System.Directory.removeFile newPath
          else liftIO $ System.Directory.removeDirectoryRecursive newPath
    )
      `catch` handleError path

  findFile name =
    ( do
        curPath <- ask
        subDirs <- liftIO $ getSubdirsRecursive curPath
        liftIO $ findFiles (curPath : subDirs) name
    )
      `catch` handleError name
    where
      getSubdirsRecursive :: FilePath -> IO [FilePath]
      getSubdirsRecursive curPath = do
        paths <- liftIO $ listDirectory curPath
        directSubDirs <- filterM (doesDirectoryExist . (curPath </>)) paths
        directSubDirs' <- mapM (resolvePath curPath) directSubDirs
        concat <$> ((:) directSubDirs' <$> mapM getSubdirsRecursive directSubDirs')

  info path =
    ( do
        curPath <- ask
        newPath <- liftIO $ resolvePath curPath path
        fileExists <- liftIO $ doesFileExist newPath
        if fileExists
          then do
            fInfo <- liftIO $ getFileInfo newPath
            return $ show fInfo
          else do
            dInfo <- liftIO $ getDirInfo newPath
            return $ show dInfo
    )
      `catch` handleError path
    where
      getFileInfo :: FilePath -> IO FileInfo
      getFileInfo filePath = do
        filePerms <- getPermissions filePath
        let fileType = takeExtension filePath
        fileModTime <- getModificationTime filePath
        fileSize <- getFileSize filePath
        return (FileInfo filePath filePerms fileType fileModTime fileSize)

      getDirInfo :: FilePath -> IO DirInfo
      getDirInfo dirPath = do
        dirPerms <- getPermissions dirPath
        dirSize <- getDirSize dirPath
        files <- listDirectory dirPath
        dirFiles <- length <$> filterM (doesFileExist . (dirPath </>)) files
        return (DirInfo dirPath dirPerms dirSize dirFiles)

      getDirSize :: FilePath -> IO Integer
      getDirSize dirPath = do
        isFile <- doesFileExist dirPath
        if isFile
          then do
            getFileSize dirPath
          else do
            content <- listDirectory dirPath
            sum <$> mapM (getDirSize . (dirPath </>)) content

-- | Represents information about a file
data FileInfo = FileInfo FilePath Permissions String UTCTime Integer

instance Show FileInfo where
  show (FileInfo path perms fType modTime size) =
    "path: " ++ path ++ "\n" ++
    "permissions: " ++ getPerms ++ "\n" ++
    "type: " ++ fType ++ "\n" ++
    "last mod time: " ++ show modTime ++ "\n" ++
    "size: " ++ show size ++ " bytes"
    where
      getPerms :: String
      getPerms =
        (if readable perms then "r" else "-") ++
        (if writable perms then "w" else "-") ++
        (if executable perms then "x" else "-")

-- | Represents information about a directory
data DirInfo = DirInfo FilePath Permissions Integer Int

instance Show DirInfo where
  show (DirInfo path perms size files) =
    "path: " ++ path ++ "\n" ++
    "perms: " ++ getPerms ++ "\n" ++
    "size: " ++ show size ++ "\n" ++
    "files: " ++ show files
    where
      getPerms :: String
      getPerms =
        (if readable perms then "r" else "-") ++
        (if writable perms then "w" else "-") ++
        (if executable perms then "x" else "-")

-- | Resolves the first path over the second.
-- resolvePath "a/b" "c" will produce "a/b/c"
resolvePath :: FilePath -> FilePath -> IO FilePath
resolvePath a b = canonicalizePath $ a </> b

-- | Executes the given action with the given directory as context
withCurrentDirectory :: FilePath -> RealFS a -> IO a
withCurrentDirectory path action = do
  runReaderT action path

-- | A handler for IO errors, which might occur
-- during file manipulation
handleError :: FilePath -> IOError -> RealFS a
handleError path e
  | isAlreadyExistsError e = throwM (PathAlreadyExistsException path)
  | isDoesNotExistError e = throwM (NoSuchPathException path)
  | isPermissionError e = throwM (PermissionDeniedException path)
  | otherwise =
    case ioe_type e of
      InvalidArgument -> throwM InvalidArgumentException
      _               -> throwM e
