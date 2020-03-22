@echo off

SET modification=scalar

SET mapper_name=ru.ifmo.rain.starodubtsev.mapper
SET conc_name=ru.ifmo.rain.starodubtsev.concurrent
SET mapper_dir=ru\ifmo\rain\starodubtsev\mapper

SET wd=C:\Users\fastr\IdeaProjects\java-advanced-2020
SET mod_path=%wd%\artifacts;%wd%\lib;%wd%\out\production\%mapper_name%;%wd%\out\production\%conc_name%
SET src=%wd%\my_modules\%mapper_name%
SET out=%wd%\out\production\%mapper_name%

echo Compiling...
javac --module-path %mod_path% %src%\module-info.java %src%\%mapper_dir%\*.java -d %out%

set /P salt="Salt: "
@echo on
java --module-path %mod_path% --add-modules %mapper_name% --add-modules %conc_name%^
 -m info.kgeorgiy.java.advanced.mapper^
 %modification% %mapper_name%.ParallelMapperImpl,%conc_name%.IterativeParallelism %salt%

