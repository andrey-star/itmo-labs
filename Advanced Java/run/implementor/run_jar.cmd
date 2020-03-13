@echo off
SET wd=C:\Users\fastr\IdeaProjects\java-advanced-2020

SET run=%wd%\run\implementor
SET mod_path=%run%;%wd%\artifacts;%wd%\lib

@echo on
java --module-path=%mod_path% -m ru.ifmo.rain.starodubtsev.implementor %1 %2 %3