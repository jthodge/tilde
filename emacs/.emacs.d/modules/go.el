;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; LANGUAGE SUPPORT - GO
;;; ================================================================

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(defun my/setup-go-development ()
  "Configure Go development environment for current buffer."
  ;; Enable minor modes first
  (yas-minor-mode 1)

  ;; Ensure flycheck is loaded before LSP
  (when (package-installed-p 'flycheck)
    (require 'flycheck nil t))

  ;; Configure Go-specific settings
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode t) ; Go uses tabs

  ;; Start LSP (gopls will be detected automatically)
  (lsp-deferred)

  ;; Setup completion after LSP is loaded
  (add-hook 'lsp-mode-hook
            (lambda ()
              (when (eq major-mode 'go-mode)
                ;; Ensure completion-at-point-functions is set up correctly
                (setq-local completion-at-point-functions
                            (list #'lsp-completion-at-point))

                ;; Use Corfu with Cape if available
                (when (and (package-installed-p 'corfu) (package-installed-p 'cape))
                  ;; Ensure cape is loaded before using its functions
                  (require 'cape nil t)
                  ;; Add cape functions to enhance completion
                  (when (fboundp 'cape-yasnippet)
                    (add-to-list 'completion-at-point-functions #'cape-yasnippet t))
                  (when (fboundp 'cape-dabbrev)
                    (add-to-list 'completion-at-point-functions #'cape-dabbrev t))
                  (when (fboundp 'cape-file)
                    (add-to-list 'completion-at-point-functions #'cape-file t)))))
            nil t)

  ;; Setup debugging with Delve
  (when (package-installed-p 'dap-mode)
    (require 'dap-dlv-go nil t)
    (dap-mode 1)))

(add-hook 'go-mode-hook #'my/setup-go-development)
