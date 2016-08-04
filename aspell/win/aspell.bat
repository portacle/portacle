@echo off
SETLOCAL

set "SCRIPT=%~dp0"
set "ROOT=%SCRIPT%\..\..\"

rem // This crazy thing resolves the relative path
FOR /F "delims=" %%F IN ("%ROOT%") DO SET "ROOT=%%~fF"

ASPELL_SHARE=%ROOT%\aspell\share
set "PATH=%ROOT%\usr\lib\;%ROOT%\aspell\win\bin\;%PATH%"
set "ASPELL_CONF=conf-dir %ASPELL_SHARE%;data-dir %ASPELL_SHARE%;home-dir %ROOT%\config"
set "ASPELL_CONF=%ASPELL_CONF:\=\\%"

"%SCRIPT%\bin\aspell" %*
ENDLOCAL
