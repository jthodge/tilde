;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; GUI PATH IMPORT
;;; ================================================================
;;
;; A GUI Emacs on macOS inherits the launchd environment, not a login
;; shell. That leaves PATH, CPATH, and LIBRARY_PATH stale and hides
;; tools installed under Homebrew, Cargo, uv, or Nix.
;;
;; exec-path-from-shell fixes that by shelling out once at startup. We
;; import only the three build/tool-discovery variables. We do not
;; import shell secrets, no matter what the user has in their profile.
;;
;; The import runs only in a graphical frame, and never under
;; noninteractive (batch) Emacs, so `emacs --batch' tests stay hermetic.

(defvar my/exec-path-from-shell-variables
  '("PATH" "CPATH" "LIBRARY_PATH")
  "Environment variables imported from the login shell in GUI Emacs.
Deliberately narrow: no secrets, no arbitrary shell state.")

(defun my/import-shell-env ()
  "Import PATH/CPATH/LIBRARY_PATH from the login shell.
Runs in GUI Emacs only, and only when exec-path-from-shell is
installed. In terminal Emacs the shell PATH is already inherited,
so no import is needed and none is performed."
  (interactive)
  (cond
   (noninteractive
    (message "Skipping shell env import in batch mode."))
   ((not (display-graphic-p))
    (message "Skipping shell env import in terminal Emacs."))
   ((not (package-installed-p 'exec-path-from-shell))
    (message "exec-path-from-shell not installed; run M-x my/install-packages."))
   (t
    (require 'exec-path-from-shell)
    (setq exec-path-from-shell-variables my/exec-path-from-shell-variables
          exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize))))

;; Run automatically at startup when in a GUI. Batch tests never enter
;; this branch and never touch exec-path-from-shell.
(when (and (display-graphic-p) (not noninteractive))
  (my/import-shell-env))
