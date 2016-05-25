(load-library "iso-transl")

(setq portacl-root (or (getenv "ROOT") (expand-file-name "~/")))
(setq user-emacs-directory (concat portacl-root "/emacs/config/"))
(add-to-list 'load-path (concat user-emacs-directory "shinmera/"))
;; Fix ECB being annoying
(custom-set-variables '(ecb-options-version "2.40"))

(require 'shinmera-general)
(require 'shinmera-lisp)
(require 'shinmera-ecb)
(require 'shinmera-paste)
(require 'shinmera-keys)
(require 'shinmera-startup)

;; Adjust defaults to fit portable stuff
(setq slime-lisp-implementations
      `((sbcl (,(concat portacl-root (cond ((eql system-type 'gnu/linux)
                                            "/sbcl/lin/sbcl.sh")
                                           ((eql system-type 'darwin)
                                            "/sbcl/mac/sbcl.sh")
                                           ((eql system-type 'windows-nt)
                                            "/sbcl/win/sbcl.bat")))))))

;; Load user file
(load (concat portacl-root "config.el"))

(startup-shinmera)
