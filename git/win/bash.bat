@echo off
SETLOCAL

set SCRIPT=%~dp0
set ROOT=%SCRIPT%\..\..\

rem // This crazy thing resolves the relative path
FOR /F "delims=" %%F IN ("%ROOT%") DO SET "ROOT=%%~fF"

set BASHPATH=%BASHPATH%;%ROOT%\usr\lib;%ROOT%\usr\bin
set PATH=%BASHPATH%

"%ROOT%\usr\bin\chroot.exe" "--skip-chdir" %ROOT% "/usr/bin/bash.exe" %*
ENDLOCAL
