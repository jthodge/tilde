;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; LANGUAGE SUPPORT - GO
;;; ================================================================
;;
;; Completion is configured centrally in `modules/lsp.el' via
;; `my/lsp-completion-setup' on `lsp-mode-hook'. This module does not
;; add its own completion callback.

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(defun my/setup-go-development ()
  "Configure Go development environment for current buffer."
  (yas-minor-mode 1)
  (when (package-installed-p 'flycheck)
    (require 'flycheck nil t))

  ;; Go-specific indent behavior.
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode t)

  (lsp-deferred)

  (when (package-installed-p 'dap-mode)
    (require 'dap-dlv-go nil t)
    (dap-mode 1)))

(add-hook 'go-mode-hook #'my/setup-go-development)
