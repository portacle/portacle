(in-package #:cl-user)
(defpackage #:portacle
  (:use #:cl)
  (:export #:portacle-root))
(in-package #:portacle)

(require 'sb-posix)

(defvar *platform*
  #+linux "lin"
  #+win32 "win"
  #+darwin "mac")

(defun portacle-root ()
  (or (sb-posix:getenv "ROOT") (user-homedir-pathname)))

(defun portacle-path (file &optional (platform *platform*))
  (merge-pathnames (format NIL "~@[~a/~]~a" platform file) (portacle-root)))

(defun load-portacle-file (file &optional (platform *platform*))
  (let ((file (portacle-path file platform)))
    (when (probe-file file) (load file))))

;; Ensure nice debuggability
(sb-ext:restrict-compiler-policy 'debug 3)

;; Fix up the source locations
(sb-ext:set-sbcl-source-location (portacle-path "sbcl/share/src/"))

;; Load ASDF
#-asdf3
(or (load-portacle-file "asdf/asdf.fasl")
    (load-portacle-file "asdf/asdf.lisp")
    (warn "Failed to load ASDF."))

;; Fix up the ASDF cache location
#+asdf3
(setf asdf:*user-cache*
      (merge-pathnames (format NIL "~a-~a-~a-~a/"
                               (lisp-implementation-type)
                               (lisp-implementation-version)
                               (software-type)
                               (machine-type))
                       (portacle-path "asdf/cache/")))

;; Load quicklisp
#-quicklisp
(or (load-portacle-file "quicklisp/setup.lisp" "all")
    (warn "Failed to load quicklisp."))

;; Add the project folder to Quicklisp's local-projects directories.
#+quicklisp
(pushnew (portacle-path "projects/" NIL) ql:*local-project-directories*)

(in-package #:cl-user)
