(load-library "iso-transl")
(load-library "portacle")

;; Set up paths
(setq user-emacs-directory (portacle-path "all/emacsd/"))
(add-to-list 'load-path user-emacs-directory)
(cd portacle-root)

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
(os-case
 (windows-nt)
 (t (require 'shinmera-spell)))

;; Customise the PATH envvar
(add-to-path (portacle-app-path "bin"))
(add-to-path (portacle-app-path "lib"))
(add-to-path (portacle-app-path "git" "bin/")
             (portacle-app-path "git" "libexec/git-core/"))

;; Make sure SLIME knows about our SBCL
(setenv "SBCL_HOME" (portacle-app-path "sbcl" "lib/sbcl/"))
(setq slime-lisp-implementations
      `((sbcl ,(portacle-os-path "bin/sbcl"))))

;; Set the Magit executable explicitly
(setenv "XDG_CONFIG_HOME" (portacle-path "config"))
(setq magit-git-executable
      (os-case (windows-nt  (portacle-app-path "git" "bin/git"))
               (t           (portacle-app-path "git" "git.sh"))))

;; Customise graphic mode
(when window-system
  (toggle-frame-maximized)
  (set-frame-font (os-case (gnu/linux "Monospace-10")
                           (darwin "Monaco-10")
                           (windows-nt "Consolas-10")) nil t))

;; Open the help file
(with-current-buffer (get-buffer-create "*portacle-help*")
  (insert-file-contents (portacle-path "config/help.txt"))
  (read-only-mode)
  (emacs-lock-mode 'kill))

(defun portacle-help ()
  (interactive)
  (switch-to-buffer (get-buffer "*portacle-help*")))

(define-my-key "C-h h" 'portacle-help)

;; Customise the scratch buffer
(setq initial-scratch-message (portacle-fread (portacle-path "config/scratch.txt")))
(setq initial-major-mode 'common-lisp-mode)

;; Populate default MC lists
(unless (file-exists-p mc/list-file)
  (setq mc/cmds-to-run-for-all
        '(backward-sexp
          downcase-region
          electric-newline-and-maybe-indent
          end-of-buffer
          forward-sexp
          indent-for-tab-command
          kill-region
          paredit-backslash
          paredit-backward
          paredit-close-round
          paredit-close-square
          paredit-comment-dwim
          paredit-convolute-sexp
          paredit-doublequote
          paredit-forward
          paredit-forward-barf-sexp
          paredit-forward-delete
          paredit-forward-down
          paredit-forward-slurp-sexp
          paredit-kill
          paredit-newline
          paredit-open-round
          paredit-open-square
          paredit-reindent-cl-defun
          paredit-semicolon
          paredit-splice-sexp-killing-backward
          paredit-backslash
          reindent-then-newline-and-indent
          scroll-other-window
          slime-autodoc-space
          slime-space
          switch-to-buffer
          upcase-region
          yank-rectangle))
  (setq mc/cmds-to-run-once
        '(down-list
          ido-list-directory
          mouse-drag-mode-line)))

;; Other fixes
(when (eql system-type 'windows-nt)
  ;; We hack this to never error because otherwise emacs refuses to work
  ;; as a server on Windows due to requiring the dir being fixed to a
  ;; "safe" directory, which we cannot ensure in our portable environment.
  (cl-defun server-ensure-safe-dir (dir)
    (unless (file-exists-p dir)
      (make-directory dir t))))

;; Load user file
(when (file-exists-p (portacle-path "config/user.el"))
  (load (portacle-path "config/user.el")))

;; Trigger contrib startup
(startup-shinmera)
