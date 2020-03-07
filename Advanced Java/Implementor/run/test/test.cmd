@echo off

SET task=%1
SET class=%2
SET modification=%3
SET module=ru.ifmo.rain.starodubtsev.%task%

SET wd=C:\Users\fastr\IdeaProjects\java-advanced-2020
SET mod_path=%wd%\artifacts;%wd%\lib;%wd%\out\production\%module%

set /P salt="Enter salt: "

@echo on
java --module-path %mod_path% --add-modules %module% -m info.kgeorgiy.java.advanced.%task% %modification% %module%.%class% %salt%

@echo off
cd %wd%