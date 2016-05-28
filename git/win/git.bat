@echo off
set SCRIPT=%~dp0
set ROOT=%SCRIPT%\..\..\

rem // This crazy thing resolves the relative path
FOR /F "delims=" %%F IN ("%ROOT%") DO SET "ROOT=%%~fF"
    
set PATH=%PATH%;%ROOT%\git\win\libexec\git-core

"%SCRIPT%\bin\git" %*
