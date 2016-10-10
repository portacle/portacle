(provide 'portacle)
(require 'cl-lib)

(cl-defmacro os-case (&body cases)
  `(cond ,@(cl-loop for case in cases collect
                    (if (eql (car case) t)
                        `(t ,@(cdr case))
                        `((eql system-type ',(car case)) ,@(cdr case))))))

(setq portacle-root (or (getenv "ROOT") (expand-file-name "~/")))
(setq portacle-os (os-case (gnu/linux "lin") (darwin "mac") (windows-nt "win")))

(defun portacle-path (path)
  (concat portacle-root path))

(defun portacle-app-path (app path)
  (portacle-path (concat app "/" portacle-os "/" path)))

(defun portacle-fwrite (contents file &optional append)
  (write-region contents nil file append))

(defun portacle-fread (file)
  (with-temp-buffer
      (insert-file-contents file)
    (buffer-string)))

;; Our update command
(defun portacle-pull-preserving-changes (place)
  (let ((default-directory place))
    (call-process magit-git-executable nil (current-buffer) t "stash")
    (call-process magit-git-executable nil (current-buffer) t "pull")
    (call-process magit-git-executable nil (current-buffer) t "stash" "pop")))

(defun portacle-recompile (&optional force)
  (byte-recompile-directory (portacle-path "config/") 0 force)
  (byte-recompile-directory (portacle-path "emacs/config/shinmera") 0 force))

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
      (insert "  --> Recompiling ELISP sources\n")
      (portacle-recompile)
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
      (insert "\n Please restart Portacle for the changes to take full effect.\n")
      (insert "\n Press q to close this buffer."))))

;; Configuration of user variables
(setq project-default-licence "BSD-3")

(cl-defun portacle-configure (&key name email licence)
  (interactive)
  (let ((name (or name (read-string "Your name: " user-full-name)))
        (email (or email (read-string "Your e-mail address: " user-mail-address)))
        (licence (or licence (read-string "Default project licence: " project-default-licence))))
    (call-process magit-git-executable nil nil t "config" "--file" (portacle-path "config/git/config") "user.name" name)
    (call-process magit-git-executable nil nil t "config" "--file" (portacle-path "config/git/config") "user.email" email)
    (portacle-fwrite (prin1-to-string `(setq user-full-name ,name)) (portacle-path "config/user.el") t)
    (portacle-fwrite (prin1-to-string `(setq user-mail-address ,email)) (portacle-path "config/user.el") t)
    (portacle-fwrite (prin1-to-string `(setq project-default-licence ,email)) (portacle-path "config/user.el") t)
    (setq user-full-name name)
    (setq user-mail-address email)
    (message "User information set.")))

;; Project management
(defun replace-project-variables (string &optional vars)
  (let ((vars '(user-full-name user-mail-address
                project-name project-description
                project-licence year month day))
        (year (format-time-string "%Y"))
        (month (format-time-string "%m"))
        (day (format-time-string "%d"))
        ;; Case-sensitive regexp mathcing
        (case-fold-search nil))
    (dolist (var vars string)
      (setq string (replace-regexp-in-string (upcase (symbol-name var)) (symbol-value var) string t t)))))

(defun maybe-update-quicklisp-db ()
  (interactive)
  (cond ((slime-connected-p)
         (message "Updating Quicklisp DB...")
         (slime-eval '(ql:register-local-projects))
         (message "Quicklisp DB updated."))
        (t
         (message "Slime not connected, cannot update."))))

(defun --copy-project-internal (from to)
  (make-directory to t)
  (dolist (template (directory-files from))
    (unless (or (string= template ".")
                (string= template ".."))
      (let ((srcfile (concat from "/" template))
            (destfile (concat to "/" (replace-project-variables template))))
        (cond ((file-directory-p srcfile)
               (--copy-project-internal srcfile destfile))
              (t
               (portacle-fwrite (replace-project-variables (portacle-fread srcfile))
                                destfile)))))))

(cl-defun create-project (&key name description licence)
  (interactive)
  (let* ((project-name (or name (read-string "Project name: ")))
         (project-description (or description (read-string "Project description: ")))
         (project-licence (or licence (read-string "Project licence: " project-default-licence)))
         (dir (portacle-path (concat "projects/" project-name)))
         (skeleton (portacle-path "config/skeleton/")))
    (cond ((file-exists-p dir)
           (message "A project with that name already exists."))
          (t
           (message "Creating project skeleton...")
           (--copy-project-internal skeleton dir)
           (magit-init dir)
           (maybe-update-quicklisp-db)
           (message "Project created.")))))

(defun clone-project (&optional url name)
  (interactive)
  (let* ((url (or url (read-string "Project URL: ")))
         (path (url-filename (url-generic-parse-url url)))
         (name (or name (car (last (split-string path "/")))))
         (dir (portacle-path (concat "projects/" name))))
    (cond ((file-exists-p dir)
           (message "A project with that name already exists."))
          (t
           (message "Cloning project...")
           (magit-clone url dir)
           (maybe-update-quicklisp-db)
           (message "Project cloned.")))))

(defun remove-project (&optional name)
  (interactive)
  (let* ((name (or name (read-string "Project name: ")))
         (dir (portacle-path (concat "projects/" name))))
    (cond ((file-exists-p dir)
           (message "Deleting project directory...")
           (delete-directory dir t)
           (maybe-update-quicklisp-db)
           (message "Project removed."))
          (t
           (message "No such project found.")))))
