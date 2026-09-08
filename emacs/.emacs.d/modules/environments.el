;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; UV PYTHON ENVIRONMENT MANAGEMENT
;;; ================================================================

(defun my/find-python-in-venv (venv-path)
  "Find Python executable in VENV-PATH, returning relative path or nil."
  (when (file-directory-p venv-path)
    (let ((possible-paths '("bin/python" "bin/python3" "base/bin/python" "base/bin/python3")))
      (seq-find (lambda (exec-path)
                  (file-exists-p (expand-file-name exec-path venv-path)))
                possible-paths))))

(defun my/activate-venv (venv-path python-rel-path)
  "Activate virtual environment at VENV-PATH with PYTHON-REL-PATH."
  (let ((python-path (expand-file-name python-rel-path venv-path)))
    (setq python-shell-interpreter python-path)
    (let ((venv-bin-dir (file-name-directory python-path)))
      (setq exec-path (cons venv-bin-dir (remove venv-bin-dir exec-path))))
    (setenv "PATH" (concat (file-name-directory python-path) path-separator (getenv "PATH")))
    (setenv "VIRTUAL_ENV" venv-path)
    (setenv "PYTHONHOME" nil)
    (message "Activated UV Python environment at %s (using %s)" venv-path python-path)))

(defun uv-activate ()
  "Activate Python environment managed by uv based on current project directory.
Looks for .venv directory in project root and activates the Python interpreter.
Falls back to $HOME/.venv if no project-specific environment is found."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (project-venv-path (expand-file-name ".venv" project-root))
         (home-venv-path (expand-file-name ".venv" (getenv "HOME")))
         (project-python-rel-path (my/find-python-in-venv project-venv-path))
         (home-python-rel-path (my/find-python-in-venv home-venv-path)))

    (cond
     (project-python-rel-path
      (my/activate-venv project-venv-path project-python-rel-path))
     (home-python-rel-path
      (my/activate-venv home-venv-path home-python-rel-path))
     (t
      (error "No Python interpreter found in %s or %s venv directories" project-root (getenv "HOME"))))))

(defun my/deactivate-current-venv ()
  "Deactivate current virtual environment."
  (when-let ((current-venv (getenv "VIRTUAL_ENV")))
    (let ((bin-dir (expand-file-name "bin" current-venv))
          (base-bin-dir (expand-file-name "base/bin" current-venv)))
      (setq exec-path (seq-remove (lambda (path) (or (string= path bin-dir) (string= path base-bin-dir))) exec-path))
      (let ((path-elements (split-string (getenv "PATH") path-separator)))
        (setenv "PATH" (mapconcat 'identity
                                  (seq-filter (lambda (path)
                                                (not (or (string= path bin-dir) (string= path base-bin-dir))))
                                              path-elements)
                                  path-separator))))
    (setq python-shell-interpreter "python")
    (setenv "VIRTUAL_ENV" nil)
    (message "Deactivated virtual environment: %s" current-venv)))

(defun uv-deactivate ()
  "Deactivate the current Python virtual environment.
If a project-specific environment is active, deactivate it and
fall back to $HOME/.venv if it exists.
Otherwise, perform default deactivation behavior."
  (interactive)
  (let* ((home-venv-path (expand-file-name ".venv" (getenv "HOME")))
         (home-python-rel-path (my/find-python-in-venv home-venv-path)))

    (my/deactivate-current-venv)

    (when (and (file-directory-p home-venv-path) home-python-rel-path)
      (my/activate-venv home-venv-path home-python-rel-path))))
