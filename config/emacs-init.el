(load-library "iso-transl")
(load-library "portacle")


;; Set up paths
(setq user-emacs-directory (portacle-path "all/emacsd/"))
(add-to-list 'load-path (portacle-path "all/emacsd/shinmera/"))
(cd portacle-root)

(unless (locate-library "shinmera")
  (display-warning :warning "Basic Portacle scripts are not present."))

(when (locate-library "shinmera")
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
  
  ;; Make sure SLIME knows about our SBCL
  (setq slime-lisp-implementations
        `((sbcl (,(portacle-bin-path "sbcl")))))

  ;; Make sure SLIME stores the FASLs within Portacle
  ;; @Override
  (defun slime-init-command (port-filename _coding-system)
    "Return a string to initialize Lisp."
    (let ((loader (if (file-name-absolute-p slime-backend)
                      slime-backend
                      (concat slime-path slime-backend))))
      ;; Return a single form to avoid problems with buffered input.
      (format "%S\n\n"
              `(progn
                 (load ,(slime-to-lisp-filename (expand-file-name loader))
                       :verbose t)
                 (setf (symbol-value (read-from-string "swank-loader:*fasl-directory*"))
                       ,(slime-to-lisp-filename (portacle-app-path "asdf" "cache/swank/")))
                 (funcall (read-from-string "swank-loader:init"))
                 (funcall (read-from-string "swank:start-server")
                          ,(slime-to-lisp-filename port-filename))))))

  ;; Set the Magit executable explicitly
  (setq magit-git-executable (portacle-bin-path "git"))

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
  (startup-shinmera))
