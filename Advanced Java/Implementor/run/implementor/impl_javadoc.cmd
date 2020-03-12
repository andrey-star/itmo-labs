@echo off

SET wd=C:\Users\fastr\IdeaProjects\java-advanced-2020
SET mod_dir=ru\ifmo\rain\starodubtsev\implementor
SET mod_name=ru.ifmo.rain.starodubtsev.implementor

SET k_mod=info.kgeorgiy.java.advanced
SET k_implementor=%k_mod%.implementor
SET k_base=%k_mod%.base
SET k_impl_dir=%wd%\modules\%k_implementor%\info\kgeorgiy\java\advanced\implementor

SET src=%wd%\my_modules\%mod_name%
SET out=%wd%\out\production\%mod_name%
SET run=%wd%\run\implementor

SET mod_path=%wd%\artifacts;%wd%\lib;%run%

@echo on
javadoc -d javadoc -link https://docs.oracle.com/en/java/javase/11/docs/api^
 --module-path %mod_path% -private -author^
 --module-source-path %wd%\modules;%wd%\my_modules^
 --module %mod_name% %k_impl_dir%\JarImpler.java %k_impl_dir%\ImplerException.java


