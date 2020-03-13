@echo off

SET cd=%CD%

SET mod_dir=ru\ifmo\rain\starodubtsev\implementor
SET mod_name=ru.ifmo.rain.starodubtsev.implementor

SET wd=C:\Users\fastr\IdeaProjects\java-advanced-2020
SET mod_path=%wd%\artifacts;%wd%\lib

SET src=%wd%\my_modules\%mod_name%
SET out=%wd%\out\production\%mod_name%
SET run=%wd%\run\implementor

javac --module-path %mod_path% %src%\module-info.java %src%\%mod_dir%\*.java -d %out%
cd %out%
jar -c --file=%run%\implementor.jar --main-class=%mod_name%.Implementor --module-path=%mod_path% module-info.class %mod_dir%\*.class
cd %cd%