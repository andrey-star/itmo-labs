# Simple File Manager

A command-line utility for managing files.

Launch: `file-manager <base-path>`

```
Usage:  COMMAND
Simple command line file manager

Available options:
-h,--help                Show this help text

Available commands:
dir                      List files in the current folder
ls                       List files in the specified folder
create-folder            Create folder with the specified name
cat                      Display file contents
create-file              Create file with the specified name in the current
folder
remove                   Remove specified file or folder
write-file               Write contents to specified file
find-file                Find file in this folder and subfolders
info                     Display info about the specifed file/folder
cd                       Change directory to the specified path
exit                     Exit the file manager
```

*Does not support quoted paths as such:* `"a\name with spaces \c"`