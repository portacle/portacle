@echo off
SETLOCAL
set ARGS=%*
set ARGS=%ARGS:$=`$%

set SCRIPT=%~dp0
set ROOT=%SCRIPT%
"%ROOT%/emacs/win/emacs.bat" %ARGS
ENDLOCAL
