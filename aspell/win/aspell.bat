@echo off
SETLOCAL

set SCRIPT=%~dp0
set ROOT=%SCRIPT%\..\..\

rem // This crazy thing resolves the relative path
FOR /F "delims=" %%F IN ("%ROOT%") DO SET "ROOT=%%~fF"

set PATH=%ROOT%\usr\lib\;%ROOT%\aspell\win\bin\;%PATH%
set ASPELL_CONF="conf-dir %ROOT%/aspell/share;home-dir %ROOT%/config;data-dir %ROOT%/aspell/lin/data"

"%SCRIPT%\bin\aspell" %*
ENDLOCAL
