@echo off
echo.
echo Il est %time:~0,2% heures %time:~3,2% minutes %time:~6,2% secondes
echo.
..\interpreter.exe tinyraytracer.scm
echo.
echo Il est %time:~0,2% heures %time:~3,2% minutes %time:~6,2% secondes
echo.
pause
