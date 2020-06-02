@echo off

SET out=%1
SET src=%2
SET bank=%3
SET mod_path=%4

echo Compiling...
RD /S /Q %out% 2> nul
mkdir %out%
javac --module-path %mod_path% -d %out%^
 %src%\module-info.java^
 %bank%\main\Server.java %bank%\main\Client.java^
 %bank%\main\bank\*.java^
 %bank%\test\*.java