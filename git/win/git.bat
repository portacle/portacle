@echo off
SETLOCAL
set SCRIPT=%~dp0
set ROOT=%SCRIPT%\..\..\
set ARGS=%*
set ARGS=%ARGS:$=`$%

rem // This crazy thing resolves the relative path
FOR /F "delims=" %%F IN ("%ROOT%") DO SET "ROOT=%%~fF"

set PATH=%ROOT%\git\win\bin;%ROOT%\git\win\libexec\git-core;%ROOT%\usr\lib;%ROOT%\usr\bin

"%ROOT%\usr\bin\chroot.exe" "--skip-chdir" %ROOT% "/git/win/bin/git.exe" %ARGS
ENDLOCAL
