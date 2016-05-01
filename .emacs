(load-library "iso-transl")

(setq portacl-root (or (getenv "ROOT") (expand-file-name "~/")))
(setq user-emacs-directory (concat portacl-root "/emacs/config/"))
(add-to-list 'load-path (concat user-emacs-directory "shinmera/"))

(require 'shinmera-general)
(require 'shinmera-lisp)
(require 'shinmera-ecb)
(require 'shinmera-paste)
(require 'shinmera-keys)
(require 'shinmera-startup)

(setq slime-lisp-implementations
      `((sbcl (,(concat portacl-root (cond ((eql system-type 'gnu/linux)
                                            "/sbcl/lin/sbcl.sh")
                                           ((eql system-type 'gnu/linux)
                                            "/sbcl/mac/sbcl.sh")
                                           ((eql system-type 'gnu/linux)
                                            "/sbcl/win/sbcl.bat")))))))
