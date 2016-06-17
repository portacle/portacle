(require 'cl)
(load-library "iso-transl")

(cl-defmacro os-case (&body cases)
  `(cond ,@(cl-loop for case in cases collect
                    (if (eql (car case) t)
                        `(t ,@(cdr case))
                        `((eql system-type ',(car case)) ,@(cdr case))))))

(defun file-contents (file)
  (with-temp-buffer
      (insert-file-contents file)
    (buffer-string)))

;; Set up paths
(setq portacle-root (or (getenv "ROOT") (expand-file-name "~/")))
(setq portacle-os (os-case (gnu/linux "lin") (darwin "mac") (windows-nt "win")))

(defun portacle-path (path)
  (concat portacle-root path))

(defun portacle-app-path (app path)
  (portacle-path (concat app "/" portacle-os "/" path)))

(setq user-emacs-directory (portacle-path "emacs/config/"))
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
(setenv "SBCL_HOME" (portacle-app-path "sbcl" "lib/sbcl/"))
(setq slime-lisp-implementations `((sbcl (,(os-case (gnu/linux (portacle-app-path "sbcl" "sbcl.sh"))
                                                    (t         (portacle-app-path "sbcl" "bin/sbcl")))))))

(when window-system
  (toggle-frame-maximized)
  ;; Pick an acceptable default font
  (set-frame-font (os-case (gnu/linux "Monospace-10")
                           (darwin "Monaco-10")
                           (windows-nt "Consolas-10")) nil t))

;; Open the help file
(with-current-buffer (get-buffer-create "*portacle-help*")
  (insert-file-contents (portacle-path "config/help.txt"))
  (read-only-mode)
  (emacs-lock-mode 'kill))

;; Customise the scratch buffer
(setq initial-scratch-message (file-contents (portacle-path "config/scratch.txt")))
(setq initial-major-mode 'common-lisp-mode)

;; Make sure we have our paths set up
(add-to-path (portacle-path "usr/bin/"))
(add-to-path (portacle-path "usr/lib/"))
(add-to-path (portacle-app-path "git" "bin/")
             (portacle-app-path "git" "libexec/git-core/"))

;; But just to make doubly sure we'll tell Magit explicitly
(setq magit-git-executable (os-case (gnu/linux (portacle-app-path "git" "git.sh"))
                                    (t         (portacle-app-path "git" "bin/git"))))

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
      (portacle-pull-preserving-changes portacle-root)
      (insert "  --> Updating config via GIT\n")
      (portacle-pull-preserving-changes (portacle-path "emacs/config/shinmera"))
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

;; Other fixes
(when (eql system-type 'windows-nt)
  ;; We hack this to never error because otherwise emacs refuses to work
  ;; as a server on Windows due to requiring the dir being fixed to a
  ;; "safe" directory, which we cannot ensure in our portable environment.
  (defun server-ensure-safe-dir (dir)
    nil))

;; Load user file
(when (file-exists-p (portacle-path "config/user.el"))
  (load (portacle-path "config/user.el")))

;; Trigger contrib startup
(startup-shinmera)
