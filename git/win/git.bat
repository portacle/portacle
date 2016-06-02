@echo off
set SCRIPT=%~dp0
set ROOT=%SCRIPT%\..\..\

rem // This crazy thing resolves the relative path
FOR /F "delims=" %%F IN ("%ROOT%") DO SET "ROOT=%%~fF"
    
set PATH=%ROOT%\git\win\bin;%ROOT%\git\win\libexec\git-core;%PATH%
set PATH=%ROOT%\shared\lib;%PATH%
set PATH=%ROOT%\shared\bin;%PATH%
set CURL_CA_BUNDLE=%ROOT%\shared\ssl\ca-bundle.crt

"%ROOT%git\win\bin\git.exe" %*
