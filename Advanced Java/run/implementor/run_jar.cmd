echo %~dp0..\..\

@echo off

SET wd=%~dp0..\..\
SET run=%wd%\run\implementor
SET mod_path=%run%;%wd%\artifacts;%wd%\lib

@echo on
java --module-path=%mod_path% -m ru.ifmo.rain.starodubtsev.implementor %1 %2 %3