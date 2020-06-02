@echo off

SET module=%1
SET class=%2
SET modification=%3

SET mod_name=ru.ifmo.rain.starodubtsev.%module%
SET mod_dir=ru\ifmo\rain\starodubtsev\%module%

SET wd=%~dp0..\..\
SET mod_path=%wd%\lib;%wd%\out\production
SET src=%wd%\my_modules\%mod_name%
SET out=%wd%\out\production\%mod_name%

echo Compiling...

rem javac --module-path %mod_path% %wd%\modules\info.kgeorgiy.java.advanced.%module%\module-info.java^
rem %wd%\modules\info.kgeorgiy.java.advanced.%module%\info\kgeorgiy\java\advanced\%module%\*.java^
rem -d %wd%\out\production\info.kgeorgiy.java.advanced.%module%

javac --module-path %mod_path% %src%\module-info.java %src%\%mod_dir%\HelloNonblockingUDPServer.java -d %out%

rem set /P salt="Salt: "
@echo on

java --module-path %mod_path% --add-modules %mod_name%^
 -m info.kgeorgiy.java.advanced.%module%/info.kgeorgiy.java.advanced.%module%.Tester^
 %modification% %mod_name%.%class%
