;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; LANGUAGE SUPPORT - JAVASCRIPT/TYPESCRIPT
;;; ================================================================
;;
;; Completion is configured centrally in `modules/lsp.el' via
;; `my/lsp-completion-setup' on `lsp-mode-hook'. This module does not
;; add its own completion callback.

;;; ----------------------------------------------------------------
;;; TypeScript language server discovery
;;; ----------------------------------------------------------------
;;
;; Prefer the project-local `typescript-language-server' when a
;; project ships one under node_modules/.bin. Fall back to the
;; executable found on exec-path. lsp-mode's own `lsp-package-path'
;; is only consulted if neither is available.
;;
;; Variable names are inspected from the installed lsp-mode:
;;   - `lsp-clients-typescript-tls-path'
;;     path/name of the typescript-language-server executable.
;;   - `lsp-clients-typescript-prefer-use-project-ts-server'
;;     when non-nil, lsp-mode resolves the project's own tsserver
;;     via `node -e require.resolve("typescript")'.

(defun my/typescript-project-server ()
  "Return the project-local typescript-language-server, or nil.
Looks under node_modules/.bin from the current file's directory
upward. Never contacts the network."
  (when buffer-file-name
    (let ((dir (locate-dominating-file
                buffer-file-name
                (lambda (d)
                  (file-executable-p
                   (expand-file-name
                    "node_modules/.bin/typescript-language-server" d))))))
      (when dir
        (expand-file-name
         "node_modules/.bin/typescript-language-server" dir)))))

(defun my/typescript-configure-server ()
  "Point lsp-mode at the best available typescript-language-server.
Preference order: project-local under node_modules/.bin, then the
executable on exec-path. If neither exists, warn once and leave
lsp-mode's defaults in place so the user sees a clear failure
rather than a silent one."
  (let* ((project (my/typescript-project-server))
         (fallback (executable-find "typescript-language-server"))
         (chosen (or project fallback)))
    ;; Set locals even before lsp-javascript has loaded its defcustoms.
    ;; A project server must never become another buffer's global default.
    (setq-local lsp-clients-typescript-prefer-use-project-ts-server t)
    (cond
     (chosen
      (setq-local lsp-clients-typescript-tls-path chosen)
      (message "typescript-language-server: %s" chosen))
     (t
      (kill-local-variable 'lsp-clients-typescript-tls-path)
      (message
       "typescript-language-server not found on PATH or in node_modules/.bin")))))

(defun my/setup-typescript-development ()
  "Configure TypeScript/JavaScript development environment for current buffer."
  (yas-minor-mode 1)
  (when (package-installed-p 'flycheck)
    (require 'flycheck nil t))
  ;; Discover the TS server before starting LSP so the client sees
  ;; the right executable on its first connection.
  (my/typescript-configure-server)
  (lsp-deferred)
  ;; Optional debug adapter, only wired up if dap-mode is installed.
  (when (package-installed-p 'dap-mode)
    (require 'dap-node nil t)))

(add-hook 'typescript-ts-mode-hook #'my/setup-typescript-development)
(add-hook 'tsx-ts-mode-hook #'my/setup-typescript-development)
(add-hook 'js-ts-mode-hook #'my/setup-typescript-development)
