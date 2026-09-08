;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; LANGUAGE SUPPORT - PYTHON
;;; ================================================================

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(setq python-indent-offset 4)

(defun my/setup-python-development ()
  "Configure Python development environment for current buffer."
  ;; Enable minor modes first
  (yas-minor-mode 1)

  ;; Ensure flycheck is loaded before LSP
  (when (package-installed-p 'flycheck)
    (require 'flycheck nil t))

  ;; Configure Python interpreter
  (setq-local python-shell-interpreter (executable-find "python"))

  ;; Start LSP
  (require 'lsp-pyright) ; TODO: Consider replacing with mypy-based LSP alternative
  (lsp-deferred)

  ;; Setup completion after LSP is loaded
  (add-hook 'lsp-mode-hook
            (lambda ()
              (when (eq major-mode 'python-mode)
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

  ;; Setup debugging
  (require 'dap-python)
  (setq-local dap-python-debugger 'debugpy)
  (dap-mode 1))

(add-hook 'python-mode-hook #'my/setup-python-development)
