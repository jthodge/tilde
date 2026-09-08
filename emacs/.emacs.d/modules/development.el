;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; DEVELOPMENT TOOLS - GENERAL
;;; ================================================================

(require 'proj-context)

;; Corfu (modern completion framework)
(when (package-installed-p 'corfu)
  ;; Load Corfu immediately and configure it
  (require 'corfu)

  (setq corfu-cycle t                    ; Enable cycling for `corfu-next/previous'
        corfu-auto t                     ; Enable auto completion
        corfu-auto-delay 0.1             ; Auto completion delay
        corfu-auto-prefix 1              ; Minimum prefix length for auto completion
        corfu-separator ?\s              ; Orderless field separator
        corfu-quit-at-boundary nil       ; Never quit at completion boundary
        corfu-quit-no-match nil          ; Never quit, even if there is no match
        corfu-preview-current 'insert    ; Preview current candidate
        corfu-preselect 'prompt          ; Preselect the prompt
        corfu-on-exact-match nil         ; Configure handling of exact matches
        corfu-scroll-margin 5            ; Use scroll margin
        corfu-count 16)                  ; Maximum number of candidates to show

  ;; Enable Corfu globally
  (global-corfu-mode 1)

  ;; Add completion keybindings
  (define-key corfu-map (kbd "TAB") #'corfu-next)
  (define-key corfu-map [tab] #'corfu-next)
  (define-key corfu-map (kbd "S-TAB") #'corfu-previous)
  (define-key corfu-map [backtab] #'corfu-previous))

;; Cape (Completion At Point Extensions)
(when (package-installed-p 'cape)
  ;; Load Cape immediately
  (require 'cape)

  ;; Add useful cape functions to the global completion-at-point-functions
  ;; These will be available as fallbacks in all buffers
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

;; Apheleia (asynchronous code formatting)
;;
;; Apheleia is the SOLE save-time formatter, including for Go. We do
;; not add `lsp-format-buffer' or `lsp-organize-imports' to
;; `before-save-hook' anywhere -- see `modules/lsp.el' for the
;; matching removal.
;;
;; The prettier command is chosen from the file's project context so
;; that a JS/TS/JSON/CSS/HTML/MD buffer in a mixed repo runs the
;; project-local prettier, not a global one, and NEVER falls back to
;; `npx' (which can install packages on save). See `proj-context.el'
;; for the resolver.
;;
;; Formatter assets (JSON, CSS, HTML, Markdown, YAML) are NOT test
;; languages -- `my/proj-context' returns `:language nil' for them so
;; `C-c t' fails closed rather than guessing. The prettier resolver
;; below therefore does its own bounded, node-modules-aware search
;; from the buffer's own directory and looks up the nearest lockfile
;; for the install-locally hint. That keeps formatting behavior
;; identical for source files and for the assets they ship with,
;; without polluting the test-command dispatcher.

(defun my/apheleia--install-hint (pm)
  "Return an install-locally hint string for package manager PM."
  (pcase pm
    (:pnpm "pnpm add -D prettier")
    (:yarn "yarn add -D prettier")
    (:npm  "npm install -D prettier")
    (_     "npm install -D prettier")))

(defun my/apheleia--prettier-search-start ()
  "Return the directory to start the prettier search from.
Uses the file's own directory when possible so JSON / CSS / HTML
/ Markdown buffers get project-local prettier even though
`my/proj-context' declines to classify them as test languages."
  (or (and buffer-file-name
           (file-name-directory (expand-file-name buffer-file-name)))
      default-directory))

(defun my/apheleia-prettier-arg1 ()
  "Return the executable name for prettier in the current buffer.

Preference order:
  1. Nearest `node_modules/.bin/prettier' at or above the file,
     bounded by the VC root. Every ancestor is inspected: a
     nested workspace whose `node_modules/' omits prettier does
     NOT block a hoisted root prettier from being picked up.
  2. `prettier' on `exec-path' (a globally installed binary is a
     safe local invocation).
  3. The literal string `prettier' so Apheleia's own
     `executable-find' guard fires and logs a skip. In that case
     we also `message' an install-locally hint chosen from the
     nearest lockfile. We deliberately do NOT fall through to
     `npx' -- that could install a package on save."
  (let* ((ctx (my/proj-context buffer-file-name))
         (vcs (plist-get ctx :vcs-root))
         (start (my/apheleia--prettier-search-start))
         (boundary (or vcs "/"))
         (local (my/proj-find-node-bin start "prettier" boundary))
         (pm (or (plist-get ctx :package-manager)
                 (my/proj--js-runner start boundary))))
    (cond
     (local local)
     ((executable-find "prettier"))
     (t
      (message
       "prettier not found in %s or on exec-path; install locally: %s"
       (or (plist-get ctx :root) start)
       (my/apheleia--install-hint pm))
      "prettier"))))

(defun my/apheleia-go-arg1 ()
  "Return the Go formatter executable for the current buffer.

Prefer `goimports' (superset of gofmt that also manages imports)
when available, else fall back to `gofmt'. Apheleia owns Go
formatting on save; LSP save-time formatting is disabled -- see
`modules/lsp.el'."
  (or (executable-find "goimports") "gofmt"))

(when (package-installed-p 'apheleia)
  (with-eval-after-load 'apheleia
    ;; Basic configuration
    (setopt apheleia-log-only-errors t          ; Only log errors, not all operations
            apheleia-hide-log-buffers t         ; Hide log buffers by default
            apheleia-formatters-respect-indent-level t) ; Respect buffer indentation

    ;; Configure Python formatters. ruff is picked from `exec-path',
    ;; so an activated project venv (see `environments.el') puts
    ;; `.venv/bin/ruff' first automatically -- the formatter context
    ;; naturally respects the buffer venv without any extra wiring.
    (setf (alist-get 'ruff apheleia-formatters)
          '("ruff" "format" "--stdin-filename" filepath "-"))
    (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)

    ;; Configure Elisp formatting
    (setf (alist-get 'emacs-lisp-mode apheleia-mode-alist) 'lisp-indent)

    ;; Configure Go formatter. Named `my/go-format' to make it
    ;; obvious in `apheleia-mode-alist' who owns Go on save.
    (setf (alist-get 'my/go-format apheleia-formatters)
          '((my/apheleia-go-arg1)))
    (setf (alist-get 'go-mode apheleia-mode-alist) 'my/go-format)
    (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'my/go-format)

    ;; Configure TypeScript/JavaScript prettier. The command list uses
    ;; a form -- `(my/apheleia-prettier-arg1)' -- which apheleia
    ;; evaluates each time it builds the formatter context, so the
    ;; resolved executable follows the buffer's project.
    (setf (alist-get 'prettier apheleia-formatters)
          '((my/apheleia-prettier-arg1) "--stdin-filepath" filepath))
    (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier)
    (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) 'prettier)
    (setf (alist-get 'js-ts-mode apheleia-mode-alist) 'prettier)
    (setf (alist-get 'json-ts-mode apheleia-mode-alist) 'prettier)

    ;; Key bindings for manual formatting
    (global-set-key (kbd "C-c f") #'apheleia-format-buffer)

    ;; Enhanced configuration
    (setopt apheleia-remote-algorithm 'cancel) ; Disable formatting for remote buffers

    ;; Enable global mode
    (apheleia-global-mode 1))

  ;; Load package
  (require 'apheleia nil t))

;; Snippets
(with-eval-after-load 'yasnippet
  (yas-reload-all))

;; Debug Adapter Protocol
(setopt dap-auto-configure-mode t)

;; Go debugging configuration
(with-eval-after-load 'dap-mode
  (require 'dap-dlv-go nil t)

  ;; Configure delve path
  (setq dap-dlv-go-delve-path (executable-find "dlv"))

  ;; Register Go debug templates
  (dap-register-debug-template
   "Go Debug"
   (list :type "go"
         :request "launch"
         :name "Go Debug"
         :mode "auto"
         :program nil
         :buildFlags nil
         :args nil
         :env nil
         :envFile nil)))
