set SCRIPT=%~dp0
set ROOT=%SCRIPT%\..\..\
set SBCL_HOME=%ROOT%\sbcl\lin\lib\sbcl\
%SCRIPT%\bin\sbcl --no-sysinit --userinit "%ROOT%\.sbclrc" $@
