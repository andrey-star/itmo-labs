@echo off
SET wd=C:\Users\fastr\IdeaProjects\java-advanced-2020

SET run=%wd%\run\implementor
SET mod_path=%run%;%wd%\artifacts;%wd%\lib

IF "%1"=="" (
    SET class=info.kgeorgiy.java.advanced.implementor.basic.classes.standard.IIOImage
) ELSE (
    SET class=%1
)

IF "%2"=="" (
    SET jar=a.jar
) ELSE (
    SET class=%2
)

@echo on
java --module-path=%mod_path% -m ru.ifmo.rain.starodubtsev.implementor --jar %class% %jar%