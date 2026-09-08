;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; LANGUAGE SUPPORT - GO
;;; ================================================================
;;
;; Completion is configured centrally in `modules/lsp.el' via
;; `my/lsp-completion-setup' on `lsp-mode-hook'. This module does not
;; add its own completion callback.

;; `go-mode' ships from MELPA. Without it installed, mapping .go to a
;; symbol that will not autoload just produces `command-execute: symbol's
;; function definition is void'. Choose a fallback so the file at least
;; opens with a usable major-mode; the language-specific setup below is
;; triggered only when the real mode is in play.
(add-to-list 'auto-mode-alist
             (cons "\\.go\\'"
                   (cond
                    ((fboundp 'go-mode) 'go-mode)
                    (t 'prog-mode))))

(defun my/setup-go-development ()
  "Configure Go development environment for current buffer.

`yas-minor-mode' and `lsp-deferred' are guarded so a fresh Emacs
without yasnippet or lsp-mode still opens Go files."
  (when (fboundp 'yas-minor-mode)
    (yas-minor-mode 1))
  (when (package-installed-p 'flycheck)
    (require 'flycheck nil t))

  ;; Go-specific indent behavior.
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode t)

  (when (fboundp 'lsp-deferred)
    (lsp-deferred))

  (when (package-installed-p 'dap-mode)
    (require 'dap-dlv-go nil t)
    (dap-mode 1)))

(add-hook 'go-mode-hook #'my/setup-go-development)
