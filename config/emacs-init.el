(load-library "iso-transl")

;; Set up paths
(setq portacl-root (or (getenv "ROOT") (expand-file-name "~/")))

(defun portacl-path (path)
  (concat portacl-root path))

(setq user-emacs-directory (portacl-path "emacs/config/"))
(add-to-list 'load-path (concat user-emacs-directory "shinmera/"))

;; Load contribs
(require 'shinmera-general)
(require 'shinmera-functions)
(require 'shinmera-neotree)
(require 'shinmera-company)
(require 'shinmera-paste)
(require 'shinmera-keys)
(require 'shinmera-magit)
(require 'shinmera-lisp)
(require 'shinmera-startup)

;; Make sure SLIME knows about our SBCL
(setq slime-lisp-implementations
      `((sbcl (,(portacl-path (cond ((eql system-type 'gnu/linux)  "sbcl/lin/sbcl.sh")
                                    ((eql system-type 'darwin)     "sbcl/mac/sbcl.sh")
                                    ((eql system-type 'windows-nt) "sbcl/win/sbcl.bat")))))))

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
  (insert-file-contents (portacl-path "config/help.txt"))
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

;; Make sure we have GIT in the path
(add-to-path (portacl-path (cond ((eql system-type 'gnu/linux)  "git/lin/bin")
                                 ((eql system-type 'darwin)     "git/mac/bin")
                                 ((eql system-type 'windows-nt) "git/win/bin")))
             (portacl-path (cond ((eql system-type 'gnu/linux)  "git/lin/libexec/git-core")
                                 ((eql system-type 'darwin)     "git/mac/libexec/git-core")
                                 ((eql system-type 'windows-nt) "git/win/libexec/git-core"))))

;; But just to make doubly sure we'll tell Magit explicitly
(setq magit-git-executable
      (portacl-path (cond ((eql system-type 'gnu/linux)  "git/lin/git.sh")
                          ((eql system-type 'darwin)     "git/mac/git.sh")
                          ((eql system-type 'windows-nt) "git/win/git.bat"))))

;; Our update command
(defun portacle-pull-preserving-changes (place)
  (let ((default-directory place))
    (call-process magit-git-executable nil (current-buffer) t "stash")
    (call-process magit-git-executable nil (current-buffer) t "pull")
    (call-process magit-git-executable nil (current-buffer) t "stash" "pop")))

(defun portacle-update ()
  (interactive)
  (with-help-window "*portacle-update*"
    (with-current-buffer "*portacle-update*"
      (switch-to-buffer (current-buffer))
      (insert "===> Starting Portacle update\n")
      (insert "  --> Updating root via GIT\n")
      (portacle-pull-preserving-changes portacl-root)
      (insert "  --> Updating config via GIT\n")
      (portacle-pull-preserving-changes (portacl-path "emacs/config/shinmera"))
      (insert "  --> Updating dists via QL\n")
      (slime-eval '(ql:update-all-dists :prompt cl:nil))
      (insert "  --> Updating client via QL\n")
      (slime-eval '(ql:update-client :prompt cl:nil))
      (insert "  --> Updating packages via ELPA\n")
      (package-refresh-contents)
      (dolist (elt package-archive-contents)
        (when (package-installed-p (car elt))
          (package-install (car elt))))
      (insert "===> All done\n")
      (insert "\n Press q to close this buffer."))))

;; Load user file
(when (file-exists-p (portacl-path "config/user.el"))
  (load (portacl-path "config/user.el")))

;; Trigger contrib startup
(startup-shinmera)
