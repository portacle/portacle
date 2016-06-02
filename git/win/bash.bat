@echo off
set SCRIPT=%~dp0
set ROOT=%SCRIPT%\..\..\

rem // This crazy thing resolves the relative path
FOR /F "delims=" %%F IN ("%ROOT%") DO SET "ROOT=%%~fF"

set PATH=%ROOT%\shared\lib;%PATH%
set PATH=%ROOT%\shared\bin;%PATH%

"%ROOT%\shared\bin\chroot.exe" %ROOT% "%ROOT%\shared\bin\bash.exe" %*
