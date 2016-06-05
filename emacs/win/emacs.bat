@echo off
SETLOCAL
set ARGS=%*
set ARGS=%ARGS:$=`$%

set SCRIPT=%~dp0
set ROOT=%SCRIPT%\..\..\

rem // This crazy thing resolves the relative path
FOR /F "delims=" %%F IN ("%ROOT%") DO SET "ROOT=%%~fF"

rem // Find emacs version
FOR /D %%F IN ("%ROOT%\emacs\share\emacs\*.*") DO SET "EMACSVER=%%~nF%%~xF" & GOTO END
:END

set PATH=%PATH%;%ROOT%\emacs\win\lib\
set EMACSDATA=%ROOT%\emacs\share\emacs\%EMACSVER%\etc\
set EMACSDOC=%ROOT%\emacs\share\emacs\%EMACSVER%\etc\
set EMACSLOADPATH=%ROOT%\emacs\share\emacs\%EMACSVER%\site-lisp;^
%ROOT%\emacs\share\emacs\site-lisp;^
%ROOT%\emacs\share\emacs\%EMACSVER%\site-lisp;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\calc;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\calendar;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\cedet;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\emacs-lisp;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\emulation;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\erc;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\eshell;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\gnus;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\international;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\language;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\mail;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\mh-e;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\net;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\nxml;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\obsolete;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\org;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\play;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\progmodes;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\term;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\textmodes;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\url;^
%ROOT%\emacs\share\emacs\%EMACSVER%\lisp\vc

set PATH=%ROOT%\emacs\win\libexec\emacs\%EMACSVER%\x86_64-w64-mingw32;%PATH%
set PATH=%ROOT%\usr\lib\;%PATH%

"%SCRIPT%\bin\emacs" --name Portacle -T Portacle -q -l "%ROOT%\config\emacs-init.el" %ARGS
ENDLOCAL
