{-# LANGUAGE RecordWildCards #-}

module Main where

import CommandParser (Command (..), parseCommand)
import Control.Monad.Catch (catch, throwM)
import FileManager (FileManagerException (..), cat, createFile, createFolder,
                    findFile, info, ls, remove, resolvePath,
                    withCurrentDirectory, writeFile)
import GHC.IO.Exception (ioe_description)
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStr "\n---> Simple File Manager <---\n\n"
  args <- getArgs
  path <- canonicalizePath (getWorkingDirectory args)
  exists <- doesDirectoryExist path
  if not exists
    then putStrLn ("Invalid base path: " ++ path)
    else do
      putStrLn ("Using base path: " ++ path)
      nextCommand path
  where
    getWorkingDirectory :: [String] -> FilePath
    getWorkingDirectory args = case args of
      [path] -> path
      _      -> "."

-- | Gets the next command from the command line
-- and executes the handler
nextCommand :: FilePath -> IO ()
nextCommand path = do
  printCmdLine path
  line <- getLine
  let cmdEither = parseCommand line
  case cmdEither of
    Left s -> do
      putStrLn s
      nextCommand path
    Right cmd -> handleCommand cmd path

-- | Handles the next command to the file manager
-- and requests the next command upon completion
handleCommand :: Command -> FilePath -> IO ()
handleCommand cmd curPath = do
  ( case cmd of
      DIR -> do
        handleCommand (LS ".") curPath
      LS {..} -> do
        fps <- exec (ls path)
        printFilePaths fps
        nextCommand curPath
      CREATE_FOLDER {..} -> do
        exec (createFolder name)
        nextCommand curPath
      CAT {..} -> do
        contents <- exec (cat path)
        putStr contents
        putStr "\n"
        nextCommand curPath
      CREATE_FILE {..} -> do
        exec (createFile name)
        nextCommand curPath
      WRITE_FILE {..} -> do
        exec (FileManager.writeFile path content)
        nextCommand curPath
      REMOVE {..} -> do
        exec (remove path)
        nextCommand curPath
      FIND_FILE {..} -> do
        fps <- exec (findFile name)
        printFilePaths fps
        nextCommand curPath
      INFO {..} -> do
        contents <- exec (info path)
        putStr contents
        putStr "\n"
        nextCommand curPath
      CD {..} -> do
        newPath <- resolvePath curPath path
        exists <- doesDirectoryExist newPath
        if exists
          then do
            nextCommand newPath
          else throwM (NoSuchPathException path)
      EXIT -> return ()
    )
    `catch` handleFMException
    `catch` handleIOError
  where
    exec = withCurrentDirectory curPath

    handleFMException :: FileManagerException -> IO ()
    handleFMException e = do
      print e
      nextCommand curPath

    handleIOError :: IOError -> IO ()
    handleIOError e = putStrLn (ioe_description e)

-- | Prints a list of file paths on separate lines
printFilePaths :: [FilePath] -> IO ()
printFilePaths = mapM_ printStrNoQuotes

-- | Prints a string without quotes
printStrNoQuotes :: String -> IO ()
printStrNoQuotes = putStr . (++ "\n")

-- | Prints the command line prefix
printCmdLine :: FilePath -> IO ()
printCmdLine path = do
  putStr ("\n" ++ path ++ ">")
  hFlush stdout
