;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; LANGUAGE SUPPORT - PYTHON
;;; ================================================================
;;
;; Completion is configured centrally in `modules/lsp.el' via
;; `my/lsp-completion-setup' on `lsp-mode-hook'. This module does not
;; add its own completion callback.

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(setq python-indent-offset 4)

(defun my/python-maybe-activate-project-venv ()
  "Activate the project .venv before Python LSP starts, if present.
Never prompts. If there is no current project, or the project has
no usable .venv, leave the buffer's Python state alone. The user
can still call `uv-activate' manually for the $HOME fallback."
  (when (fboundp 'uv-activate-project-buffer)
    (uv-activate-project-buffer)))

(defun my/setup-python-development ()
  "Configure Python development environment for current buffer."
  (yas-minor-mode 1)
  (when (package-installed-p 'flycheck)
    (require 'flycheck nil t))

  ;; Buffer-local Python interpreter default. `my/python-maybe-activate-project-venv'
  ;; will override this to the venv interpreter when a project .venv exists.
  (setq-local python-shell-interpreter
              (or (executable-find "python")
                  (executable-find "python3")
                  "python"))

  ;; Activate project .venv before starting LSP so pyright sees the
  ;; correct interpreter on its first connection.
  (my/python-maybe-activate-project-venv)

  ;; Start LSP. lsp-pyright is optional; if the package is not
  ;; installed we let plain lsp-mode try whatever Python client is
  ;; registered rather than crashing on `require'.
  (when (package-installed-p 'lsp-pyright)
    (require 'lsp-pyright nil t))
  (lsp-deferred)

  ;; Optional debugger; keep the whole block guarded so a missing
  ;; dap-mode does not break Python editing.
  (when (package-installed-p 'dap-mode)
    (require 'dap-python nil t)
    (setq-local dap-python-debugger 'debugpy)
    (dap-mode 1)))

(add-hook 'python-mode-hook #'my/setup-python-development)
