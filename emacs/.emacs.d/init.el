;; init.el  -*- lexical-binding: t; -*-

;;; ================================================================
;;; BOOTSTRAP
;;; ================================================================

;; Suppress obsolete warnings from package dependencies
;; N.B. Upstream `cl` is the offending dependency package
(setq byte-compile-warnings '(not obsolete))

;; Add cargo bin to exec-path for tools like emacs-lsp-booster
(let ((cargo-bin (expand-file-name "~/.cargo/bin")))
  (when (file-directory-p cargo-bin)
    (add-to-list 'exec-path cargo-bin)))

(load "~/.emacs.d/scripts.el")

;;; ================================================================
;;; CORE UI CONFIGURATION
;;; ================================================================

;; Basic UI cleanup
(menu-bar-mode 0)
(setq inhibit-startup-screen t)
(column-number-mode)
(global-display-line-numbers-mode)

;; GUI-specific settings
(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(undecorated . t))
  (tool-bar-mode 0)
  (scroll-bar-mode 0))

;; Modern scrolling for Emacs 29+
(when (version<= "29" emacs-version)
  (pixel-scroll-precision-mode 1))

;; Theme and font
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'ujelly t)

(when (member "Berkeley Mono" (font-family-list))
  (set-frame-font "Berkeley Mono" t t))

;;; ================================================================
;;; EDITING BEHAVIOR
;;; ================================================================

;; Whitespace and indentation
(setq-default show-trailing-whitespace t
              indicate-empty-lines 1
              indicate-buffer-boundaries 'left
              indent-tabs-mode nil
              tab-width 4
              require-final-newline t)

;; Text formatting
(setq sentence-end-double-space nil)

;; Parentheses highlighting
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Minibuffer completion
(when (package-installed-p 'vertico)
  (with-eval-after-load 'vertico
    (vertico-mode))
  (require 'vertico nil t))

(when (package-installed-p 'orderless)
  (require 'orderless nil t)
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

(when (package-installed-p 'marginalia)
  (with-eval-after-load 'marginalia
    (marginalia-mode))
  (require 'marginalia nil t))

;;; ================================================================
;;; PACKAGE MANAGEMENT
;;; ================================================================

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defconst my-packages
  '(apheleia               ; Asynchronous code formatting
    cape                   ; Completion At Point Extensions for Corfu
    consult                ; Incremental narrowing
    consult-lsp            ; Improve working between `consult` and `lsp-mode`
    corfu                  ; Modern completion frontend
    dap-mode               ; Debug Adapter Protocol Support
    flycheck               ; Linting and syntax checker
    lsp-mode               ; Language Server Protocol support
    lsp-pyright            ; Language Server Protocol client using pyright Python Language Server
    lsp-ui                 ; UI improvements for `lsp-mode`
    marginalia             ; Rich completion annotations
    orderless              ; Flexible completion matching
    vertico                ; Modern vertical completion
    which-key              ; Display currently available keybindings
    yasnippet              ; Snippet and template management
    ))

(mapc (lambda (package)
        (unless (package-installed-p package)
          (condition-case err
              (package-install package)
            (error
             (message "Failed to install package %s: %s" package err)))))
      my-packages)

;;; ================================================================
;;; PERFORMANCE OPTIMIZATIONS
;;; ================================================================

;; Increase the amount of data Emacs reads from processes
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Optimize garbage collection thresholds
(setq gc-cons-threshold 100000000) ;; 100mb
(setq gc-cons-percentage 0.5)

;; Configure native compilation if available
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil))

;;; ================================================================
;;; LANGUAGE SERVER PROTOCOL (LSP)
;;; ================================================================

;;; ----------------------------------------------------------------
;;; Core LSP Configuration
;;; ----------------------------------------------------------------

;; Disable plists for compatibility with updated lsp-mode
;; The updated lsp-mode may have changed plist handling
(setenv "LSP_USE_PLISTS" nil)

;; Ensure lsp-use-plists is disabled
(with-eval-after-load 'lsp-protocol
  (setq lsp-use-plists nil))

;; Also set it if lsp-protocol is already loaded
(when (featurep 'lsp-protocol)
  (setq lsp-use-plists nil))

;; Core LSP Mode configuration
(setopt lsp-keymap-prefix "C-c l"
        lsp-keep-workspace-alive nil
        lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)
        lsp-ui-doc-delay 0.5
        lsp-diagnostics-provider :flycheck
        lsp-restart 'auto-restart
        lsp-server-install-dir (expand-file-name "lsp-servers/" user-emacs-directory)
        ;; Performance tuning
        lsp-idle-delay 0.5
        lsp-log-io nil  ; Disable IO logging for performance (set to t for debugging)
        lsp-completion-provider :capf  ; Use completion-at-point (works with Corfu)
        lsp-prefer-flymake nil  ; Use flycheck
        lsp-enable-file-watchers nil  ; Disable file watchers for performance
        lsp-enable-folding nil  ; Disable folding for performance
        lsp-enable-text-document-color nil  ; Disable color info
        lsp-enable-on-type-formatting nil  ; Disable on-type formatting
        lsp-enable-indentation nil  ; Let Apheleia handle formatting
        ;; Explicitly disable company-mode integration
        lsp-completion-enable t
        lsp-enable-snippet t
        lsp-completion-show-detail t
        lsp-completion-show-kind t)

;;; ----------------------------------------------------------------
;;; LSP Performance Optimization
;;; ----------------------------------------------------------------

;; TODO: Install and configure emacs-lsp-booster for improved LSP performance
;; emacs-lsp-booster provides significant performance improvements by:
;; - Converting JSON responses to bytecode for faster parsing
;; - Reducing CPU usage during LSP communication
;; - Improving overall responsiveness
;;
;; Installation:
;; 1. Install via cargo: cargo install emacs-lsp-booster
;; 2. Ensure ~/.cargo/bin is in PATH or exec-path
;; 3. Configure advice functions to wrap LSP commands
;; 4. Test compatibility with current lsp-mode version
;;
;; Temporarily disabled due to compatibility issues with updated lsp-mode package.
;; The booster converts messages to plist format, but the new lsp-mode (20250730.1549)
;; expects hash-table format, causing "wrong-type-argument hash-table-p" errors.
;; This breaks all LSP communication and diagnostic display.

;;; ----------------------------------------------------------------
;;; LSP UI Configuration
;;; ----------------------------------------------------------------

;; LSP UI key remapping
(with-eval-after-load 'lsp-ui
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;;; ----------------------------------------------------------------
;;; LSP Integration with Other Tools
;;; ----------------------------------------------------------------

;; Configure file watching after lsp-mode loads
(with-eval-after-load 'lsp-mode
  ;; Ensure flycheck is loaded before lsp-diagnostics
  (when (package-installed-p 'flycheck)
    (require 'flycheck nil t))
  ;; Ensure lsp-diagnostics is loaded for flycheck faces
  (require 'lsp-diagnostics)

  ;; File watching ignore patterns
  (setq lsp-file-watch-ignored-directories
        (append lsp-file-watch-ignored-directories
                '("[/\\\\]\\.Trash\\'"
                  "[/\\\\]\\.git\\'"
                  "[/\\\\]\\.venv\\'"
                  "[/\\\\]__pycache__\\'"
                  "[/\\\\]node_modules\\'"
                  "[/\\\\]\\.DS_Store\\'")))

  ;; Remove custom lsp-completion--enable function - let LSP handle completion setup
  ;; The custom function was interfering with proper completion initialization

  ;; Consult integration
  (define-key lsp-mode-map [remap lsp-treemacs-errors-list] #'consult-lsp-diagnostics)
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)

  ;; Which-key integration
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;;; ----------------------------------------------------------------
;;; Language-Specific LSP Servers
;;; ----------------------------------------------------------------

;; TODO: Re-enable ESLint Language Server for TypeScript/JavaScript after Python is perfected
;; (with-eval-after-load 'lsp-mode
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-stdio-connection
;;                      (lambda ()
;;                        (list "/Users/jth/.volta/bin/vscode-eslint-language-server" "--stdio")))
;;     :activation-fn (lsp-activate-on "typescript" "javascript" "javascriptreact"
;;                                     "typescriptreact" "javascript.jsx" "typescript.tsx")
;;     :server-id 'eslint-lsp
;;     :priority -1
;;     :add-on? t
;;     :multi-root t
;;     :initialization-options
;;     (lambda ()
;;       (list :nodePath "/Users/jth/.volta/bin/node"
;;             :quiet :json-false
;;             :rulesCustomizations []
;;             :run "onType"
;;             :validate "on"
;;             :packageManager "npm"
;;             :codeActionOnSave (list :mode "all" :rules [])
;;             :format :json-false
;;             :onIgnoredFiles "off"
;;             :problems (list :shortenToSingleLine :json-false)
;;             :workingDirectory (list :mode "auto"))))))

;;; ================================================================
;;; TREE-SITTER CONFIGURATION
;;; ================================================================

;;; ----------------------------------------------------------------
;;; Core Tree-sitter Setup
;;; ----------------------------------------------------------------

(defun my/setup-treesitter-grammars ()
  "Install Tree-sitter grammars if they're absent."
  (interactive)
  (when (and (fboundp 'treesit-install-language-grammar)
             (boundp 'treesit-language-source-alist))
    (let ((grammars '(;; TODO: Re-enable C language support after Python is perfected
                      ;; (c "https://github.com/tree-sitter/tree-sitter-c")
                      ;; TODO: Re-enable CMake language support after Python is perfected
                      ;; (cmake "https://github.com/uyha/tree-sitter-cmake")
                      ;; TODO: Re-enable C++ language support after Python is perfected
                      ;; (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                      ;; TODO: Re-enable CSS language support after Python is perfected
                      ;; (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                      ;; TODO: Re-enable Elisp language support after Python is perfected
                      ;; (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                      ;; TODO: Re-enable HTML language support after Python is perfected
                      ;; (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                      ;; TODO: Re-enable JavaScript language support after Python is perfected
                      ;; (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
                      ;; TODO: Re-enable JSON language support after Python is perfected
                      ;; (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                      (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                      ;; TODO: Re-enable Make language support after Python is perfected
                      ;; (make "https://github.com/alemuller/tree-sitter-make")
                      ;; TODO: Re-enable Markdown language support after Python is perfected
                      ;; (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                      ;; TODO: Re-enable TOML language support after Python is perfected
                      ;; (toml "https://github.com/tree-sitter/tree-sitter-toml")
                      ;; TODO: Re-enable TSX language support after Python is perfected
                      ;; (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                      ;; TODO: Re-enable TypeScript language support after Python is perfected
                      ;; (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                      ;; TODO: Re-enable YAML language support after Python is perfected
                      ;; (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
                      )))
      (dolist (grammar grammars)
        (add-to-list 'treesit-language-source-alist grammar)
        (condition-case err
            (unless (treesit-language-available-p (car grammar))
              (treesit-install-language-grammar (car grammar)))
          (error
           (message "Failed to install tree-sitter grammar for %s: %s" (car grammar) err)))))))

;;; ----------------------------------------------------------------
;;; Mode Remapping
;;; ----------------------------------------------------------------

(defun my/setup-treesitter-mode-remapping ()
  "Configure major mode remapping for tree-sitter modes."
  (when (and (fboundp 'treesit-available-p)
             (boundp 'major-mode-remap-alist))
    (let ((mode-mappings '(;; TODO: Re-enable TypeScript mode remapping after Python is perfected
                           ;; (typescript-mode . typescript-ts-mode)
                           ;; TODO: Re-enable JavaScript mode remapping after Python is perfected
                           ;; (js-mode . typescript-ts-mode)
                           ;; (js2-mode . typescript-ts-mode)
                           ;; TODO: Re-enable JSON mode remapping after Python is perfected
                           ;; (json-mode . json-ts-mode)
                           ;; (js-json-mode . json-ts-mode)
                           )))
      (dolist (mapping mode-mappings)
        (add-to-list 'major-mode-remap-alist mapping)))))

;;; ----------------------------------------------------------------
;;; File Associations
;;; ----------------------------------------------------------------

(defun my/setup-treesitter-auto-modes ()
  "Configure file associations for tree-sitter modes."
  (when (fboundp 'treesit-available-p)
    (let ((file-associations '(;; TODO: Re-enable TSX file associations after Python is perfected
                               ;; ("\\.tsx\\'" . tsx-ts-mode)
                               ;; TODO: Re-enable JavaScript file associations after Python is perfected
                               ;; ("\\.js\\'" . typescript-ts-mode)
                               ;; ("\\.mjs\\'" . typescript-ts-mode)
                               ;; ("\\.mts\\'" . typescript-ts-mode)
                               ;; ("\\.cjs\\'" . typescript-ts-mode)
                               ;; TODO: Re-enable TypeScript file associations after Python is perfected
                               ;; ("\\.ts\\'" . typescript-ts-mode)
                               ;; ("\\.jsx\\'" . tsx-ts-mode)
                               ;; TODO: Re-enable JSON file associations after Python is perfected
                               ;; ("\\.json\\'" . json-ts-mode)
                               ;; TODO: Re-enable Dockerfile associations after Python is perfected
                               ;; ("\\.Dockerfile\\'" . dockerfile-ts-mode)
                               )))
      (dolist (association file-associations)
        (add-to-list 'auto-mode-alist association)))))

;;; ----------------------------------------------------------------
;;; Initialize Tree-sitter
;;; ----------------------------------------------------------------

(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (my/setup-treesitter-grammars)
  (my/setup-treesitter-mode-remapping)
  (my/setup-treesitter-auto-modes))

;;; ================================================================
;;; DEVELOPMENT TOOLS - GENERAL
;;; ================================================================

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
(when (package-installed-p 'apheleia)
  (with-eval-after-load 'apheleia
    ;; Basic configuration
    (setopt apheleia-log-only-errors t          ; Only log errors, not all operations
            apheleia-hide-log-buffers t         ; Hide log buffers by default
            apheleia-formatters-respect-indent-level t) ; Respect buffer indentation

    ;; Configure Python formatters
    (setf (alist-get 'ruff apheleia-formatters)
          '("ruff" "format" "--stdin-filename" filepath "-"))

    ;; Configure mode associations for Python files
    (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)

    ;; TODO: Re-enable non-Python formatters after Python is perfected
    ;; (setf (alist-get 'prettier-volta apheleia-formatters)
    ;;       '("/Users/jth/.volta/bin/prettier"
    ;;         "--stdin-filepath" filepath
    ;;         (apheleia-formatters-locate-file ".prettierrc.js" ".prettierrc.json" ".prettierrc.yml" ".prettierrc.yaml" ".prettierrc")))
    ;; (setf (alist-get 'eslint-volta apheleia-formatters)
    ;;       '("/Users/jth/.volta/bin/eslint"
    ;;         "--stdin-filename" filepath
    ;;         "--fix-dry-run"
    ;;         "--format" "json"
    ;;         "--stdin"))
    ;; (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier-volta)
    ;; (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) 'prettier-volta)
    ;; (setf (alist-get 'typescript-mode apheleia-mode-alist) 'prettier-volta)
    ;; (setf (alist-get 'js-mode apheleia-mode-alist) 'prettier-volta)
    ;; (setf (alist-get 'js2-mode apheleia-mode-alist) 'prettier-volta)
    ;; (setf (alist-get 'javascript-mode apheleia-mode-alist) 'prettier-volta)
    ;; (setf (alist-get 'json-mode apheleia-mode-alist) 'prettier-volta)
    ;; (setf (alist-get 'json-ts-mode apheleia-mode-alist) 'prettier-volta)

    ;; Key bindings for manual formatting
    (global-set-key (kbd "C-c f") #'apheleia-format-buffer)

    ;; Enhanced configuration
    (setopt apheleia-remote-algorithm 'cancel) ; Disable formatting for remote buffers

    ;; Enable global mode
    (apheleia-global-mode 1))

  ;; Load the package
  (require 'apheleia nil t))

;; Snippets
(with-eval-after-load 'yasnippet
  (yas-reload-all))

;; Debug Adapter Protocol
(setopt dap-auto-configure-mode t)

;;; ================================================================
;;; KEYBINDINGS
;;; ================================================================

;; Org mode keybindings (prevent conflicts with lsp-mode)
(global-set-key (kbd "C-c o l") #'org-store-link)
(global-set-key (kbd "C-c o b") #'org-switchb)

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
  (require 'lsp-pyright) ; TODO: Consider replacing with mypy-based LSP alternative for better type checking
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

;;; ================================================================
;;; LANGUAGE SUPPORT - TYPESCRIPT/JAVASCRIPT (DISABLED FOR PYTHON FOCUS)
;;; ================================================================

;; TODO: Re-enable TypeScript/JavaScript language support after Python is perfected
;; (defun my/setup-typescript-development ()
;;   "Configure TypeScript/JavaScript development environment for current buffer."
;;   ;; Use Corfu with Cape if available
;;   (when (and (package-installed-p 'corfu) (package-installed-p 'cape))
;;     ;; Ensure cape is loaded before using its functions
;;     (require 'cape nil t)
;;     ;; Add yasnippet support via Cape only if the function exists
;;     (when (fboundp 'cape-yasnippet)
;;       (add-to-list 'completion-at-point-functions #'cape-yasnippet t)))

;;   (lsp-deferred)
;;   (yas-minor-mode 1)
;;   (when (fboundp 'dap-mode)
;;     (dap-mode 1)))

;; (add-hook 'typescript-ts-mode-hook #'my/setup-typescript-development)
;; (add-hook 'tsx-ts-mode-hook #'my/setup-typescript-development)
;; (add-hook 'typescript-mode-hook #'my/setup-typescript-development)

;;; ================================================================
;;; ================================================================
;;; UV PYTHON ENVIRONMENT MANAGEMENT
;;; ================================================================

(defun my/find-python-in-venv (venv-path)
  "Find Python executable in VENV-PATH, returning relative path or nil."
  (when (file-directory-p venv-path)
    (let ((possible-paths '("bin/python" "bin/python3" "base/bin/python" "base/bin/python3")))
      (seq-find (lambda (exec-path)
                  (file-exists-p (expand-file-name exec-path venv-path)))
                possible-paths))))

(defun my/activate-venv (venv-path python-rel-path)
  "Activate virtual environment at VENV-PATH with PYTHON-REL-PATH."
  (let ((python-path (expand-file-name python-rel-path venv-path)))
    (setq python-shell-interpreter python-path)
    (let ((venv-bin-dir (file-name-directory python-path)))
      (setq exec-path (cons venv-bin-dir (remove venv-bin-dir exec-path))))
    (setenv "PATH" (concat (file-name-directory python-path) path-separator (getenv "PATH")))
    (setenv "VIRTUAL_ENV" venv-path)
    (setenv "PYTHONHOME" nil)
    (message "Activated UV Python environment at %s (using %s)" venv-path python-path)))

(defun uv-activate ()
  "Activate Python environment managed by uv based on current project directory.
Looks for .venv directory in project root and activates the Python interpreter.
Falls back to $HOME/.venv if no project-specific environment is found."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (project-venv-path (expand-file-name ".venv" project-root))
         (home-venv-path (expand-file-name ".venv" (getenv "HOME")))
         (project-python-rel-path (my/find-python-in-venv project-venv-path))
         (home-python-rel-path (my/find-python-in-venv home-venv-path)))

    (cond
     (project-python-rel-path
      (my/activate-venv project-venv-path project-python-rel-path))
     (home-python-rel-path
      (my/activate-venv home-venv-path home-python-rel-path))
     (t
      (error "No Python interpreter found in %s or %s venv directories" project-root (getenv "HOME"))))))

(defun my/deactivate-current-venv ()
  "Deactivate current virtual environment."
  (when-let ((current-venv (getenv "VIRTUAL_ENV")))
    (let ((bin-dir (expand-file-name "bin" current-venv))
          (base-bin-dir (expand-file-name "base/bin" current-venv)))
      (setq exec-path (seq-remove (lambda (path) (or (string= path bin-dir) (string= path base-bin-dir))) exec-path))
      (let ((path-elements (split-string (getenv "PATH") path-separator)))
        (setenv "PATH" (mapconcat 'identity
                                  (seq-filter (lambda (path)
                                                (not (or (string= path bin-dir) (string= path base-bin-dir))))
                                              path-elements)
                                  path-separator))))
    (setq python-shell-interpreter "python")
    (setenv "VIRTUAL_ENV" nil)
    (message "Deactivated virtual environment: %s" current-venv)))

(defun uv-deactivate ()
  "Deactivate the current Python virtual environment.
If a project-specific environment is active, deactivate it and
fall back to $HOME/.venv if it exists.
Otherwise, perform default deactivation behavior."
  (interactive)
  (let* ((home-venv-path (expand-file-name ".venv" (getenv "HOME")))
         (home-python-rel-path (my/find-python-in-venv home-venv-path)))

    (my/deactivate-current-venv)

    (when (and (file-directory-p home-venv-path) home-python-rel-path)
      (my/activate-venv home-venv-path home-python-rel-path))))

;;; ================================================================
;;; CUSTOM SETTINGS
;;; ================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f747c4004e38bcdc131649a90325c00d246bb7dc73bc6ab6e0e7ab5489da8459"
     "a4340c197a450c77c729cad236b5f3ca88aaf974e91a7af2d2e7ae7bb5f96720"
     "6b20d669fcbcd79c6d0f3db36a71af1b88763246d3550a0c361866adecb38a9e"
     default))
 '(package-selected-packages nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; ================================================================
;;; EXTERNAL INTEGRATIONS
;;; ================================================================

;; Development testing utilities
(load-file "~/.emacs.d/test-lsp-booster.el")
(load-file "~/.emacs.d/test-corfu-migration.el")
(load-file "~/.emacs.d/test-apheleia.el")
;; TODO: Re-enable non-Python test files after Python is perfected
;; (load-file "~/.emacs.d/test-eslint-lsp.el")
