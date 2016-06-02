@echo off
SETLOCAL
set SCRIPT=%~dp0
set ROOT=%SCRIPT%\..\..\

rem // This crazy thing resolves the relative path
FOR /F "delims=" %%F IN ("%ROOT%") DO SET "ROOT=%%~fF"

set BASHPATH=%BASHPATH%;%ROOT%\shared\lib;%ROOT%\shared\bin
set PATH=%BASHPATH%

"%ROOT%\shared\bin\chroot.exe" %ROOT% "/shared/bin/bash.exe" %*
ENDLOCAL
