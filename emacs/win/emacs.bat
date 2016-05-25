set SCRIPT=%~dp0
set ROOT=%SCRIPT%\..\..\

set PATH=$PATH$;%ROOT%\emacs\lin\lib\
set EMACSDATA=%ROOT%\emacs\share\emacs\25.0.93\etc\
set EMACSDOC=%ROOT%\emacs\share\emacs\25.0.93\etc\
set EMACSLOADPATH=%ROOT%\emacs\share\emacs\25.0.93\site-lisp:^
%ROOT%\emacs\share\emacs\site-lisp:^
%ROOT%\emacs\share\emacs\25.0.93\site-lisp:^
%ROOT%\emacs\share\emacs\25.0.93\lisp:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\calc:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\calendar:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\cedet:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\emacs-lisp:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\emulation:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\erc:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\eshell:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\gnus:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\international:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\language:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\mail:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\mh-e:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\net:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\nxml:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\obsolete:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\org:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\play:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\progmodes:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\term:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\textmodes:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\url:^
%ROOT%\emacs\share\emacs\25.0.93\lisp\vc

set PATH=%PATH%;%ROOT%\emacs\win\libexec\emacs\25.0.93\x86_64-w64-mingw32

%SCRIPT%\bin\emacs --name Portacle -T Portacle -q -l "%ROOT%\.emacs" %*
