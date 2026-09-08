;;; init.el --- Explicit configuration load order -*- lexical-binding: t; -*-

;; Set custom-file early so Customize writes go to an ignored local file,
;; never into this init file or the tracked custom-settings module.
;; Durable Customize declarations live in modules/custom-settings.el; that
;; module is tracked. The local custom-file is loaded last, after all
;; modules, so it can override anything for the machine only.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Keep recovery files out of projects without disabling autosave or backups.
(let ((backups (expand-file-name "backups/" user-emacs-directory))
      (autosaves (expand-file-name "auto-save/" user-emacs-directory)))
  (make-directory backups t)
  (make-directory autosaves t)
  (setq backup-by-copying t
        backup-directory-alist `(("." . ,backups))
        auto-save-file-name-transforms `((".*" ,autosaves t))))

;; Keep order explicit: package setup and hooks depend on earlier modules.
;; environment (GUI PATH import) and environments (Python venv helpers)
;; both load before any language module that consults exec-path or
;; process-environment.
(dolist (module '("bootstrap"
                  "packages"
                  "environment"
                  "interface"
                  "lsp"
                  "treesitter"
                  "development"
                  "bindings"
                  "environments"
                  "typescript"
                  "python"
                  "go"
                  "elisp"
                  "custom-settings"))
  (load (expand-file-name (concat "modules/" module ".el") user-emacs-directory)
        nil nil t))

;; Load machine-local Custom last so it can override module defaults.
;; The file is gitignored; its absence is normal on a fresh checkout.
(when (file-readable-p custom-file)
  (load custom-file nil nil t))
