@echo off

SET module=%1
SET class=%2
SET modification=%3

SET mod_name=ru.ifmo.rain.starodubtsev.%module%
SET mod_dir=ru\ifmo\rain\starodubtsev\%module%

SET wd=C:\Users\fastr\IdeaProjects\java-advanced-2020
SET mod_path=%wd%\lib;%wd%\out\production
SET src=%wd%\my_modules\%mod_name%
SET out=%wd%\out\production\%mod_name%

echo Compiling...
javac --module-path %mod_path% %src%\module-info.java %src%\%mod_dir%\*.java -d %out%

javac --module-path %mod_path% %wd%\modules\info.kgeorgiy.java.advanced.%module%\module-info.java^
 %wd%\modules\info.kgeorgiy.java.advanced.%module%\info\kgeorgiy\java\advanced\%module%\*.java^
 -d %wd%\out\production\info.kgeorgiy.java.advanced.%module%


rem set /P salt="Salt: "
@echo on

java --module-path %mod_path% --add-modules %mod_name% --add-modules %mod_name%^
 -m info.kgeorgiy.java.advanced.%module%/info.kgeorgiy.java.advanced.%module%.Tester^
 %modification% %mod_name%.%class%
