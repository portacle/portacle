@echo off
SETLOCAL

set SCRIPT=%~dp0
set ROOT=%SCRIPT%\..\..\

rem // This crazy thing resolves the relative path
FOR /F "delims=" %%F IN ("%ROOT%") DO SET "ROOT=%%~fF"

set PATH=%ROOT%\git\win\bin;%ROOT%\git\win\libexec\git-core;%ROOT%\usr\win\lib;%ROOT%\usr\win\bin
set XDG_CONFIG_HOME=%ROOT%\config

"%ROOT%\usr\win\bin\chroot.exe" "--skip-chdir" %ROOT% "/git/win/bin/git.exe" %*
ENDLOCAL
