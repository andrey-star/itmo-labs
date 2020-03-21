@echo off

SET modification=scalar

SET mod_name=ru.ifmo.rain.starodubtsev.mapper
SET mod_dir=ru\ifmo\rain\starodubtsev\mapper

SET wd=C:\Users\fastr\IdeaProjects\java-advanced-2020
SET mod_path=%wd%\artifacts;%wd%\lib;%wd%\out\production
SET src=%wd%\my_modules\%mod_name%
SET out=%wd%\out\production\%mod_name%

echo Compiling...
javac --module-path %mod_path% %src%\module-info.java %src%\%mod_dir%\*.java -d %out%

set /P salt="Salt: "
@echo on
java --module-path %mod_path% --add-modules ru.ifmo.rain.starodubtsev.mapper^
 --add-modules ru.ifmo.rain.starodubtsev.concurrent^
 -m info.kgeorgiy.java.advanced.mapper/info.kgeorgiy.java.advanced.mapper.Tester %modification%^
 ru.ifmo.rain.starodubtsev.mapper.ParallelMapperImpl,ru.ifmo.rain.starodubtsev.concurrent.IterativeParallel