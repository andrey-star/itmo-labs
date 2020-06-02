@echo off

SET mod_dir=ru\ifmo\rain\starodubtsev\bank
SET mod_name=ru.ifmo.rain.starodubtsev.bank

SET wd=%~dp0..\..
SET src=%wd%\my_modules\%mod_name%
SET bank=%wd%\my_modules\%mod_name%\%mod_dir%
SET out=%wd%\out\production\%mod_name%
SET run=%wd%\run\bank
SET lib=%wd%\lib

SET mod_path=%lib%

CALL %run%\compile %out% %src% %bank% "%mod_path%"

java --module-path=%mod_path%;%out% -m %mod_name%/%mod_name%.test.BankTests
echo Tests finished with exit code %errorlevel%

