-- | Contains functions, which parse user commands to the file manager
-- from the command line
module CommandParser (Command (..), parseCommand) where

import Options.Applicative (Parser, ParserFailure (..), ParserInfo,
                            ParserResult (..), command, defaultPrefs,
                            execParserPure, fullDesc, header, helper,
                            hsubparser, idm, info, infoOption, metavar,
                            progDesc, strArgument, (<**>))

-- | Represents a file manager command with arguments
data Command
  = DIR                                                 -- ^ Represents dir
  | LS {path :: FilePath}                               -- ^ Represents ls \<folder\>
  | CREATE_FOLDER {name :: FilePath}                    -- ^ Represents create-folder "folder-name"
  | CAT {path :: FilePath}                              -- ^ Represents cat \<file\>
  | CREATE_FILE {name :: FilePath}                      -- ^ Represents create-file "file-name"
  | REMOVE {path :: FilePath}                           -- ^ Represents remove \<folder | file\>
  | WRITE_FILE {path :: FilePath, content :: FilePath}  -- ^ Represents write-file <file> "text"
  | FIND_FILE {name :: FilePath}                        -- ^ Represents find-file "file-name"
  | INFO {path :: FilePath}                             -- ^ Represents information \<folder | file\>
  | CD {path :: FilePath}                               -- ^ Represents cd \<folder\>
  | EXIT                                                -- ^ Represents exit

-- | Returns the command parsed from the given string,
-- or a parser error
parseCommand :: String -> Either String Command
parseCommand s = do
  let ps = execParserPure defaultPrefs opts (words s)
  case ps of
    Success cmd -> Right cmd
    Failure (ParserFailure f) -> do
      let (h, _, _) = f ""
      Left $ show h
    CompletionInvoked _ -> Left "Unsupported operation"

opts :: ParserInfo Command
opts =
  info
    (commandParser <**> infoOption "qwe" idm <**> helper)
    ( fullDesc
        <> progDesc "Simple command line file manager"
        <> header "file manager"
    )

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "dir" (info (pure DIR) (progDesc "List files in the current folder"))
        <> command "ls" (info (LS <$> folderOptions) (progDesc "List files in the specified folder"))
        <> command "create-folder" (info (CREATE_FOLDER <$> nameOptions) (progDesc "Create folder with the specified name"))
        <> command "cat" (info (CAT <$> fileOptions) (progDesc "Display file contents"))
        <> command "create-file" (info (CREATE_FILE <$> nameOptions) (progDesc "Create file with the specified name in the current folder"))
        <> command "remove" (info (REMOVE <$> folderFileOptions) (progDesc "Remove specified file or folder"))
        <> command "write-file" (info (WRITE_FILE <$> fileOptions <*> contentOptions) (progDesc "Write contents to specified file"))
        <> command "find-file" (info (FIND_FILE <$> nameOptions) (progDesc "Find file in this folder and subfolders"))
        <> command "info" (info (INFO <$> folderFileOptions) (progDesc "Display info about the specifed file/folder"))
        <> command "cd" (info (CD <$> folderOptions) (progDesc "Change directory to the specified path"))
        <> command "exit" (info (pure EXIT) (progDesc "Exit the file manager"))
    )

folderOptions :: Parser String
folderOptions = strOption "FOLDER"

fileOptions :: Parser String
fileOptions = strOption "FILE"

folderFileOptions :: Parser String
folderFileOptions = strOption "FILE/FOLDER"

nameOptions :: Parser String
nameOptions = strOption "NAME"

contentOptions :: Parser String
contentOptions = strOption "CONTENT"

strOption :: String -> Parser String
strOption = strArgument . metavar
