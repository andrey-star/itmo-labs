@echo off

SET task=concurrent
SET class=IterativeParallelism
SET modification=scalar

SET mod_name=ru.ifmo.rain.starodubtsev.%task%
SET mod_dir=ru\ifmo\rain\starodubtsev\%task%

SET wd=C:\Users\fastr\IdeaProjects\java-advanced-2020
SET mod_path=%wd%\artifacts;%wd%\lib;%wd%\out\production\%mod_name%
SET src=%wd%\my_modules\%mod_name%
SET out=%wd%\out\production\%mod_name%

echo Compiling...
javac --module-path %mod_path% %src%\module-info.java %src%\%mod_dir%\*.java -d %out%

rem set /P salt="Salt: "
@echo on
java --module-path %mod_path% --add-modules %mod_name% -m info.kgeorgiy.java.advanced.mapper^
 %modification% %mod_name%.%class% %salt%
