(load-library "iso-transl")

;; Set up paths
(setq portacl-root (or (getenv "ROOT") (expand-file-name "~/")))
(setq user-emacs-directory (concat portacl-root "emacs/config/"))
(add-to-list 'load-path (concat user-emacs-directory "shinmera/"))

;; Load contribs
(require 'shinmera-general)
(require 'shinmera-neotree)
(require 'shinmera-company)
(require 'shinmera-paste)
(require 'shinmera-keys)
(require 'shinmera-lisp)
(require 'shinmera-startup)

;; Make sure SLIME knows about our SBCL
(setq slime-lisp-implementations
      `((sbcl (,(concat portacl-root (cond ((eql system-type 'gnu/linux)
                                            "/sbcl/lin/sbcl.sh")
                                           ((eql system-type 'darwin)
                                            "/sbcl/mac/sbcl.sh")
                                           ((eql system-type 'windows-nt)
                                            "/sbcl/win/sbcl.bat")))))))

(when window-system
  (toggle-frame-maximized)
  ;; Pick an acceptable default font
  (cond ((eql system-type 'gnu/linux)
         (set-frame-font "Monospace-10" nil t))
        ((eql system-type 'darwin)
         (set-frame-font "Monaco-10" nil t))
        ((eql system-type 'windows-nt)
         (set-frame-font "Consolas-10" nil t))))

;; Open the help file
(with-current-buffer (get-buffer-create "*portacle-help*")
  (insert-file-contents (concat portacl-root "config/help.txt"))
  (beginning-of-buffer))

;; Customise the scratch buffer
(setq initial-scratch-message "\
;;;; Welcome to Portacle, the Portable Common Lisp Environment.
;; For information on Portacle and how to use it, please read the website at
;;   https://github.com/Shinmera/portacle
;; or see the *portacle-help* buffer. You can switch to it by pressing this:
;;   Ctrl+x b *portacle-help* Enter
;;
;; You can use this buffer for notes and tinkering with small pieces of code.

")
(setq initial-major-mode 'common-lisp-mode)

;; Load user file
(load (concat portacl-root "config/user.el"))

;; Trigger contrib startup
(startup-shinmera)
