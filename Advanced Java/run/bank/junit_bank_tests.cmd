@echo off

SET mod_dir=ru\ifmo\rain\starodubtsev\bank
SET mod_name=ru.ifmo.rain.starodubtsev.bank

SET wd=%~dp0..\..
SET src=%wd%\my_modules\%mod_name%
SET bank=%wd%\my_modules\%mod_name%\%mod_dir%
SET out=%wd%\out\production\%mod_name%
SET run=%wd%\run\bank
SET lib=%wd%\lib
SET junit-standalone-jar=%lib%\standalone\junit-platform-console-standalone-1.6.1.jar

SET mod_path=%lib%

CALL %run%\compile %out% %src% %bank% "%mod_path%"

java -jar %junit-standalone-jar% --class-path %out% --scan-class-path
echo Tests finished with exit code %errorlevel%
