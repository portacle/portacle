@echo off
set SCRIPT=%~dp0
set ROOT=%SCRIPT%\..\..\

rem // This crazy thing resolves the relative path
FOR /F "delims=" %%F IN ("%ROOT%") DO SET "ROOT=%%~fF"

set BASHPATH=%ROOT%\git\win\bin;%ROOT%\git\win\libexec\git-core;%BASHPATH%

"%ROOT%git\win\bash.bat" "-c" "/git/win/bin/git.exe %*"
