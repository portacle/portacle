@echo off
set SCRIPT=%~dp0
set ROOT=%SCRIPT%\..\..\

rem // This crazy thing resolves the relative path.
FOR /F "delims=" %%F IN ("%ROOT%") DO SET "ROOT=%%~fF"

set SBCL_HOME=%ROOT%\sbcl\win\lib\sbcl\

"%SCRIPT%\bin\sbcl" --no-sysinit --userinit "%ROOT%\config\sbcl-init.lisp" $@
