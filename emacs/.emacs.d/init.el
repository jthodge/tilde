;;; init.el --- Explicit configuration load order -*- lexical-binding: t; -*-

;; Keep order explicit: package setup and hooks depend on earlier modules.
;; The first extraction preserved every original form and its order.
;; See docs/emacs-workflow.md for the verification record.
(dolist (module '("bootstrap"
                  "interface"
                  "packages"
                  "lsp"
                  "treesitter"
                  "development"
                  "bindings"
                  "typescript"
                  "python"
                  "go"
                  "elisp"
                  "environments"
                  "custom-settings"))
  (load (expand-file-name (concat "modules/" module ".el") user-emacs-directory)
        nil nil t))
