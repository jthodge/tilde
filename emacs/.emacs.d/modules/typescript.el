;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; LANGUAGE SUPPORT - JAVASCRIPT/TYPESCRIPT
;;; ================================================================

(defun my/setup-typescript-development ()
  "Configure TypeScript/JavaScript development environment for current buffer."
  ;; Enable minor modes first
  (yas-minor-mode 1)

  ;; Ensure flycheck is loaded before lsp-diagnostics
  (when (package-installed-p 'flycheck)
    (require 'flycheck nil t))

  ;; Disable company-mode integration
  (setq lsp-completion-provider :capf)

  ;; Start LSP
  (lsp-deferred)

  ;; Setup completion after LSP loaded
  (add-hook 'lsp-mode-hook
            (lambda ()
              (when (or (eq major-mode 'typescript-ts-mode)
                        (eq major-mode 'tsx-ts-mode)
                        (eq major-mode 'js-ts-mode))
                ;; Confirm completion-at-point-functions set up correctly
                (setq-local completion-at-point-functions
                            (list #'lsp-completion-at-point))

                ;; Use Corfu with Cape if available
                (when (and (package-installed-p 'corfu) (package-installed-p 'cape))
                  ;; Ensure cape loaded before using its functions
                  (require 'cape nil t)
                  ;; Add cape functions to enhance completion
                  (when (fboundp 'cape-yasnippet)
                    (add-to-list 'completion-at-point-functions #'cape-yasnippet t))
                  (when (fboundp 'cape-dabbrev)
                    (add-to-list 'completion-at-point-functions #'cape-dabbrev t))
                  (when (fboundp 'cape-file)
                    (add-to-list 'completion-at-point-functions #'cape-file t)))))
            nil t)

  ;; Setup debugging (optional - only if needed)
  ;; To install debug adapter: M-x dap-node-setup
  (when (package-installed-p 'dap-mode)
    (require 'dap-node nil t)))

;; Hook integration for TypeScript/JavaScript modes
(add-hook 'typescript-ts-mode-hook #'my/setup-typescript-development)
(add-hook 'tsx-ts-mode-hook #'my/setup-typescript-development)
(add-hook 'js-ts-mode-hook #'my/setup-typescript-development)
