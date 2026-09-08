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

(require 'proj-context)

(defun my/typescript-project-server ()
  "Return the project-local typescript-language-server, or nil.
Delegates to `my/proj-find-node-bin' so the search starts at the
buffer's own directory, is bounded by the VC root, and walks past
a nested workspace whose `node_modules/' omits the server -- a
hoisted root binary is still reachable. Never contacts the network."
  (when buffer-file-name
    (let* ((ctx (my/proj-context buffer-file-name))
           (vcs (plist-get ctx :vcs-root))
           (start (file-name-directory (expand-file-name buffer-file-name))))
      (my/proj-find-node-bin start "typescript-language-server"
                             (or vcs "/")))))

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
  "Configure TypeScript/JavaScript development environment for current buffer.

`yas-minor-mode' and `lsp-deferred' are guarded so a fresh Emacs
without yasnippet or lsp-mode still opens .ts/.tsx buffers."
  (when (fboundp 'yas-minor-mode)
    (yas-minor-mode 1))
  (when (package-installed-p 'flycheck)
    (require 'flycheck nil t))
  ;; Discover the TS server before starting LSP so the client sees
  ;; the right executable on its first connection.
  (my/typescript-configure-server)
  (when (fboundp 'lsp-deferred)
    (lsp-deferred))
  ;; Optional debug adapter, only wired up if dap-mode is installed.
  (when (package-installed-p 'dap-mode)
    (require 'dap-node nil t)))

(add-hook 'typescript-ts-mode-hook #'my/setup-typescript-development)
(add-hook 'tsx-ts-mode-hook #'my/setup-typescript-development)
(add-hook 'js-ts-mode-hook #'my/setup-typescript-development)

;;; ----------------------------------------------------------------
;;; Auto-mode fallback for .ts / .tsx / .js when tree-sitter is absent
;;; ----------------------------------------------------------------
;;
;; `treesitter.el' installs `auto-mode-alist' entries for these
;; extensions only when the corresponding grammar is available. Without
;; a grammar (fresh checkout, missing native library) those extensions
;; would fall through to `fundamental-mode' and never trigger any of
;; the hooks above. Register a usable fallback: prefer the classic
;; `typescript-mode' when the third-party package is installed,
;; otherwise `prog-mode' for basic editing. Replace an unusable built-in
;; tree-sitter mapping, but preserve unrelated custom mode choices.

(defun my/typescript--register-fallback (pattern grammar)
  "Map PATTERN to a usable mode while preserving unrelated custom mappings.
An existing built-in tree-sitter mapping is not usable without its grammar."
  (let* ((entry (assoc pattern auto-mode-alist))
         (ts-mode (if (eq grammar 'tsx) 'tsx-ts-mode 'typescript-ts-mode))
         (ready (and (fboundp 'treesit-available-p) (treesit-available-p)
                     (fboundp 'treesit-language-available-p)
                     (treesit-language-available-p grammar)
                     (fboundp ts-mode))))
    (when (or (null entry)
              (and (not ready) (memq (cdr entry) '(typescript-ts-mode tsx-ts-mode))))
      (setf (alist-get pattern auto-mode-alist nil nil #'equal)
            (cond (ready ts-mode)
                  ((fboundp 'typescript-mode) 'typescript-mode)
                  (t 'prog-mode))))))

(my/typescript--register-fallback "\\.ts\\'" 'typescript)
(my/typescript--register-fallback "\\.tsx\\'" 'tsx)
