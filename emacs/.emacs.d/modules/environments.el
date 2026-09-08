;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; PYTHON VIRTUAL ENVIRONMENT HELPERS
;;; ================================================================
;;
;; These helpers activate a per-project or fallback Python venv on
;; buffer entry, keep the changes buffer-local, and restore the
;; original state exactly on deactivation. In particular:
;;
;; - `process-environment', `exec-path', and `python-shell-interpreter'
;;   are all made buffer-local before any mutation.
;; - The pre-activation values are captured in buffer-local snapshot
;;   variables so `uv-deactivate' can restore them verbatim.
;; - Switching A -> B never accumulates path entries. Activation
;;   always deactivates any prior venv in the same buffer first, so
;;   the buffer's PATH/exec-path returns to the original snapshot
;;   before the new venv is layered on.
;; - No `project-current' prompt: when there is no current project,
;;   the interactive helper falls through to the $HOME/.venv fallback
;;   or a plain message. `uv-activate-project-buffer' is a silent,
;;   hook-safe variant.

(require 'project)
(require 'seq)

(defvar-local my/venv--snapshot nil
  "Buffer-local snapshot of pre-activation Python environment state.
Nil means no venv is currently active in this buffer. When set,
it is a plist with :process-environment, :exec-path,
:python-shell-interpreter, and :virtual-env-path.")

(defun my/find-python-in-venv (venv-path)
  "Find Python executable in VENV-PATH, returning relative path or nil."
  (when (file-directory-p venv-path)
    (let ((possible-paths '("bin/python" "bin/python3"
                            "base/bin/python" "base/bin/python3")))
      (seq-find (lambda (rel)
                  (file-executable-p (expand-file-name rel venv-path)))
                possible-paths))))

(defun my/venv--snapshot-current ()
  "Return a plist snapshot of the buffer's current env-affecting state."
  (list :process-environment (copy-sequence process-environment)
        :exec-path (copy-sequence exec-path)
        :python-shell-interpreter (and (boundp 'python-shell-interpreter)
                                       python-shell-interpreter)
        :virtual-env-path (getenv "VIRTUAL_ENV")))

(defun my/venv--restore-snapshot (snap)
  "Restore buffer-local env state from plist SNAP produced by the snapshot."
  (setq-local process-environment
              (copy-sequence (plist-get snap :process-environment)))
  (setq-local exec-path
              (copy-sequence (plist-get snap :exec-path)))
  (setq-local python-shell-interpreter
              (plist-get snap :python-shell-interpreter)))

(defun my/activate-venv (venv-path python-rel-path)
  "Activate virtual environment at VENV-PATH with PYTHON-REL-PATH.
All environment mutations are buffer-local. If a venv is already
active in this buffer it is deactivated first, so switching
A -> B does not accumulate PATH entries."
  ;; Validate before touching any state, including an existing snapshot.
  (unless (file-executable-p (expand-file-name python-rel-path venv-path))
    (user-error "Python is not executable in %s" venv-path))
  ;; Copy the lists: a local binding alone can still share mutable cons cells.
  (setq-local process-environment (copy-sequence process-environment))
  (setq-local exec-path (copy-sequence exec-path))
  (make-local-variable 'python-shell-interpreter)

  ;; If a venv is active in this buffer, roll back to its snapshot
  ;; before layering on the new one. That guarantees exact restore
  ;; semantics for A -> B -> deactivate.
  (when my/venv--snapshot
    (my/venv--restore-snapshot my/venv--snapshot)
    (setq my/venv--snapshot nil))

  ;; Snapshot the pre-activation state now.
  (setq my/venv--snapshot (my/venv--snapshot-current))

  (let* ((python-path (expand-file-name python-rel-path venv-path))
         (venv-bin-dir (file-name-directory python-path)))
    (setq-local python-shell-interpreter python-path)
    (setq-local exec-path (cons venv-bin-dir (remove venv-bin-dir exec-path)))
    ;; `setenv' operates on the buffer-local process-environment
    ;; because we made it local above.
    (setenv "PATH" (concat venv-bin-dir path-separator (getenv "PATH")))
    ;; HOME fallback can resolve .venv/base/bin/python; use its actual root.
    (setenv "VIRTUAL_ENV"
            (directory-file-name (file-name-directory
                                  (directory-file-name venv-bin-dir))))
    (setenv "PYTHONHOME" nil)
    (message "Activated Python venv at %s (using %s)" venv-path python-path)))

(defun my/venv--project-root-or-nil ()
  "Return current project root or nil, without ever prompting."
  (when-let* ((proj (project-current nil)))
    (project-root proj)))

(defun uv-activate-project-buffer ()
  "Activate the project .venv for the current buffer if one exists.
Silent, hook-safe, never prompts. Intended for `python-mode-hook'."
  (when-let* ((root (my/venv--project-root-or-nil))
              (venv (expand-file-name ".venv" root))
              (rel (my/find-python-in-venv venv)))
    (my/activate-venv venv rel)))

(defun uv-activate ()
  "Activate a Python venv for the current buffer.
Preference order: project .venv, then $HOME/.venv as an optional
manual fallback. Never prompts for a project."
  (interactive)
  (let* ((root (my/venv--project-root-or-nil))
         (project-venv (and root (expand-file-name ".venv" root)))
         (project-rel (and project-venv (my/find-python-in-venv project-venv)))
         (home-venv (expand-file-name ".venv" (getenv "HOME")))
         (home-rel (my/find-python-in-venv home-venv)))
    (cond
     (project-rel
      (my/activate-venv project-venv project-rel))
     (home-rel
      (my/activate-venv home-venv home-rel))
     (t
      (message "No .venv found in current project or in $HOME; nothing to activate.")))))

(defun my/deactivate-current-venv ()
  "Deactivate the venv active in this buffer, if any.
Restores the exact pre-activation snapshot: process-environment,
exec-path, python-shell-interpreter, and VIRTUAL_ENV. Does not
touch other buffers."
  (when my/venv--snapshot
    (let ((prev-venv (getenv "VIRTUAL_ENV")))
      (my/venv--restore-snapshot my/venv--snapshot)
      (setq my/venv--snapshot nil)
      (message "Deactivated Python venv%s"
               (if prev-venv (format ": %s" prev-venv) "")))))

(defun uv-deactivate ()
  "Deactivate the current buffer's Python venv.
No implicit fallback activation. If the user wants the $HOME
fallback back, they can call `uv-activate' explicitly."
  (interactive)
  (my/deactivate-current-venv))
