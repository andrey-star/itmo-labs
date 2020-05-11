@echo off

SET wd=%~dp0..\..
SET mod_name=ru.ifmo.rain.starodubtsev.implementor

SET k_impl_dir=%wd%\modules\info.kgeorgiy.java.advanced.implementor\info\kgeorgiy\java\advanced\implementor

SET src=%wd%\my_modules\%mod_name%
SET out=%wd%\out\production\%mod_name%
SET run=%wd%\run\implementor

SET mod_path=%wd%\artifacts;%wd%\lib

@echo on
javadoc -d javadoc -link https://docs.oracle.com/en/java/javase/11/docs/api^
 --module-path %mod_path% -private -author^
 --module-source-path %wd%\modules;%wd%\my_modules^
 --module %mod_name% %k_impl_dir%\Impler.java %k_impl_dir%\JarImpler.java %k_impl_dir%\ImplerException.java
 