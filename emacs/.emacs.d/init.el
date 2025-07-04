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

;; Completion
(ido-mode 1)
(ido-everywhere)
(setq ido-enable-flex-matching t)
(fido-mode)

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
    company                ; Code and text completion framework (legacy, will be phased out)
    consult                ; Incremental narrowing
    consult-lsp            ; Improve working between `consult` and `lsp-mode`
    corfu                  ; Modern completion frontend
    dap-mode               ; Debug Adapter Protocol Support
    flycheck               ; Linting and syntax checker
    lsp-mode               ; Language Server Protocol support
    lsp-pyright            ; Language Server Protocol client using pyright Python Language Server
    lsp-ui                 ; UI improvements for `lsp-mode`
    typescript-mode        ; TypeScript editing support (fallback for non-tree-sitter)
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

;; Set lsp-use-plists after packages are available but before lsp-mode is loaded
(with-eval-after-load 'lsp-protocol
  (setq lsp-use-plists t))

;; Also set it if lsp-protocol is already loaded
(when (featurep 'lsp-protocol)
  (setq lsp-use-plists t))

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

;; LSP Booster configuration - must be set before lsp-mode loads
;; Enable plists for better performance (required for LSP Booster)
;; IMPORTANT: This must be set before lsp-mode is loaded for the first time
;; The environment variable must be set early
(setenv "LSP_USE_PLISTS" "true")

(defun my/lsp-booster-find-executable ()
  "Find emacs-lsp-booster executable in common locations."
  (or (executable-find "emacs-lsp-booster")
      (let ((cargo-path (expand-file-name "~/.cargo/bin/emacs-lsp-booster")))
        (when (file-executable-p cargo-path)
          cargo-path))))

;; Define advice functions for LSP Booster
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json.
This allows emacs-lsp-booster to work correctly with bytecode responses."
  (or (when (equal (following-char) ?#)
        (let ((bytecode (read (current-buffer))))
          (when (byte-code-function-p bytecode)
            (funcall bytecode))))
      (apply old-fn args)))

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD when appropriate."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                                  ; Not a test run
             (not (file-remote-p default-directory))      ; Not on remote
             (or (and (boundp 'lsp-use-plists) lsp-use-plists) ; plists enabled
                 (getenv "LSP_USE_PLISTS"))               ; or env var set
             (not (functionp 'json-rpc-connection))       ; Not using json-rpc
             (my/lsp-booster-find-executable))             ; Booster is available
        (progn
          ;; Resolve the command path if needed
          (when-let ((command-from-exec-path (executable-find (car orig-result))))
            (setcar orig-result command-from-exec-path))
          (message "LSP Booster: Wrapping command %s" orig-result)
          (cons (my/lsp-booster-find-executable) orig-result))
      orig-result)))

;; Apply the advice functions
(with-eval-after-load 'lsp-mode
  ;; Add JSON parsing advice
  (advice-add (if (progn (require 'json nil t)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around #'lsp-booster--advice-json-parse)

  ;; Add command wrapping advice
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

;;; ================================================================
;;; DEVELOPMENT TOOLS - GENERAL
;;; ================================================================

;; Corfu (modern completion framework)
(when (package-installed-p 'corfu)
  (with-eval-after-load 'corfu
    (setopt corfu-cycle t           ; Enable cycling for `corfu-next/previous'
            corfu-auto t            ; Enable auto completion
            corfu-auto-delay 0.1    ; Auto completion delay
            corfu-auto-prefix 1     ; Minimum prefix length for auto completion
            corfu-separator ?\s     ; Orderless field separator
            corfu-quit-at-boundary nil   ; Never quit at completion boundary
            corfu-quit-no-match nil      ; Never quit, even if there is no match
            corfu-preview-current 'insert ; Preview current candidate
            corfu-preselect 'prompt       ; Preselect the prompt
            corfu-on-exact-match nil      ; Configure handling of exact matches
            corfu-scroll-margin 5         ; Use scroll margin
            corfu-count 16)               ; Maximum number of candidates to show

    ;; Enable Corfu globally
    (global-corfu-mode))
  
  ;; If package is installed, require it to ensure it loads
  (require 'corfu nil t))

;; Cape (Completion At Point Extensions)
(when (package-installed-p 'cape)
  (with-eval-after-load 'cape
    ;; Add useful cape functions to the completion-at-point-functions
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-elisp-block))
  
  ;; If package is installed, require it to ensure it loads
  (require 'cape nil t))

;; Apheleia (asynchronous code formatting)
(when (package-installed-p 'apheleia)
  (with-eval-after-load 'apheleia
    ;; Basic configuration
    (setopt apheleia-log-only-errors t          ; Only log errors, not all operations
            apheleia-hide-log-buffers t         ; Hide log buffers by default
            apheleia-formatters-respect-indent-level t) ; Respect buffer indentation
    
    ;; Configure Volta-managed formatters
    (setf (alist-get 'prettier-volta apheleia-formatters)
          '("/Users/jth/.volta/bin/prettier" 
            "--stdin-filepath" filepath
            (apheleia-formatters-locate-file ".prettierrc.js" ".prettierrc.json" ".prettierrc.yml" ".prettierrc.yaml" ".prettierrc")))
    
    (setf (alist-get 'eslint-volta apheleia-formatters)
          '("/Users/jth/.volta/bin/eslint"
            "--stdin-filename" filepath
            "--fix-dry-run"
            "--format" "json"
            "--stdin"))
    
    ;; Configure mode associations for TypeScript/JavaScript files
    (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier-volta)
    (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) 'prettier-volta)
    (setf (alist-get 'typescript-mode apheleia-mode-alist) 'prettier-volta)
    (setf (alist-get 'js-mode apheleia-mode-alist) 'prettier-volta)
    (setf (alist-get 'js2-mode apheleia-mode-alist) 'prettier-volta)
    (setf (alist-get 'javascript-mode apheleia-mode-alist) 'prettier-volta)
    (setf (alist-get 'json-mode apheleia-mode-alist) 'prettier-volta)
    (setf (alist-get 'json-ts-mode apheleia-mode-alist) 'prettier-volta)
    
    ;; Key bindings for manual formatting
    (global-set-key (kbd "C-c f") #'apheleia-format-buffer)
    
    ;; Enhanced configuration  
    (setopt apheleia-remote-algorithm 'cancel) ; Disable formatting for remote buffers
    
    ;; Enable global mode
    (apheleia-global-mode 1))

  ;; Configure LSP to not format on save (let Apheleia handle it)
  (with-eval-after-load 'lsp-mode
    (setq lsp-enable-on-type-formatting nil
          lsp-enable-indentation nil))
  
  ;; Load the package
  (require 'apheleia nil t))

;; Company (fallback if Corfu is not available)
(unless (package-installed-p 'corfu)
  (message "Corfu not available, falling back to Company mode")
  (require 'company)
  (setopt company-idle-delay 0.1
          company-tooltip-align-annotations t)
  (global-company-mode 1))

;; Company helper macro (for fallback mode)
(defmacro my/company-backend-for-hook (hook backends)
  "Set BACKENDS for company completion on HOOK."
  `(add-hook ,hook (lambda ()
                     (set (make-local-variable 'company-backends)
                          ,backends))))

;; Snippets
(with-eval-after-load 'yasnippet
  (yas-reload-all))

;; LSP Mode configuration
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
        lsp-enable-on-type-formatting nil)  ; Disable on-type formatting

;; Configure file watching after lsp-mode loads
(with-eval-after-load 'lsp-mode
  (setq lsp-file-watch-ignored-directories
        (append lsp-file-watch-ignored-directories
                '("[/\\\\]\\.Trash\\'"
                  "[/\\\\]\\.git\\'"
                  "[/\\\\]\\.venv\\'"
                  "[/\\\\]__pycache__\\'"
                  "[/\\\\]node_modules\\'"
                  "[/\\\\]\\.DS_Store\\'"))))

;; LSP UI key remapping
(with-eval-after-load 'lsp-ui
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

;; LSP Mode enhancements
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map [remap lsp-treemacs-errors-list] #'consult-lsp-diagnostics)
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; Debug Adapter Protocol
(setopt dap-auto-configure-mode t)

;;; ================================================================
;;; KEYBINDINGS
;;; ================================================================

;; Org mode keybindings (prevent conflicts with lsp-mode)
(global-set-key (kbd "C-c o l") #'org-store-link)
(global-set-key (kbd "C-c o b") #'org-switchb)

;;; ================================================================
;;; TREE-SITTER CONFIGURATION
;;; ================================================================

(defun my/setup-treesitter-grammars ()
  "Install Tree-sitter grammars if they're absent."
  (interactive)
  (when (and (fboundp 'treesit-install-language-grammar)
             (boundp 'treesit-language-source-alist))
    (let ((grammars '((c "https://github.com/tree-sitter/tree-sitter-c")
                      (cmake "https://github.com/uyha/tree-sitter-cmake")
                      (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                      (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                      (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                      (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                      (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
                      (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                      ;; TODO: confirm smooth dovetail with existing python lang support
                      ;; (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                      (make "https://github.com/alemuller/tree-sitter-make")
                      (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                      (toml "https://github.com/tree-sitter/tree-sitter-toml")
                      (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                      (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                      (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0")))))
      (dolist (grammar grammars)
        (add-to-list 'treesit-language-source-alist grammar)
        (condition-case err
            (unless (treesit-language-available-p (car grammar))
              (treesit-install-language-grammar (car grammar)))
          (error
           (message "Failed to install tree-sitter grammar for %s: %s" (car grammar) err)))))))

(defun my/setup-treesitter-mode-remapping ()
  "Configure major mode remapping for tree-sitter modes."
  (when (and (fboundp 'treesit-available-p)
             (boundp 'major-mode-remap-alist))
    ;; TODO: complete mode remappings
    (let ((mode-mappings '((typescript-mode . typescript-ts-mode)
                           (js-mode . typescript-ts-mode)
                           (js2-mode . typescript-ts-mode)
                           (json-mode . json-ts-mode)
                           (js-json-mode . json-ts-mode))))
      (dolist (mapping mode-mappings)
        (add-to-list 'major-mode-remap-alist mapping)))))

(defun my/setup-treesitter-auto-modes ()
  "Configure file associations for tree-sitter modes."
  (when (fboundp 'treesit-available-p)
    ;; TODO: complete file associations
    (let ((file-associations '(("\\.tsx\\'" . tsx-ts-mode)
                               ("\\.js\\'" . typescript-ts-mode)
                               ("\\.mjs\\'" . typescript-ts-mode)
                               ("\\.mts\\'" . typescript-ts-mode)
                               ("\\.cjs\\'" . typescript-ts-mode)
                               ("\\.ts\\'" . typescript-ts-mode)
                               ("\\.jsx\\'" . tsx-ts-mode)
                               ("\\.json\\'" . json-ts-mode)
                               ("\\.Dockerfile\\'" . dockerfile-ts-mode))))
      (dolist (association file-associations)
        (add-to-list 'auto-mode-alist association)))))

;; Initialize tree-sitter
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (my/setup-treesitter-grammars)
  (my/setup-treesitter-mode-remapping)
  (my/setup-treesitter-auto-modes))

;;; ================================================================
;;; LANGUAGE SUPPORT - PYTHON
;;; ================================================================

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(setq python-indent-offset 4)

(defun my/setup-python-development ()
  "Configure Python development environment for current buffer."
  ;; Use Corfu with Cape if available, otherwise fall back to Company
  (if (package-installed-p 'corfu)
      ;; Corfu setup
      (when (featurep 'cape)
        ;; Add yasnippet support via Cape
        (add-to-list 'completion-at-point-functions #'cape-yasnippet t))
    ;; Company fallback setup
    (my/company-backend-for-hook 'lsp-completion-mode-hook
                                 '((company-capf :with company-yasnippet)
                                   company-dabbrev-code)))
  
  (require 'lsp-pyright)
  (lsp-deferred)
  (yas-minor-mode 1)
  (setq-local python-shell-interpreter (executable-find "python"))
  (require 'dap-python)
  (setq-local dap-python-debugger 'debugpy)
  (dap-mode 1))

(add-hook 'python-mode-hook #'my/setup-python-development)

;;; ================================================================
;;; LANGUAGE SUPPORT - TYPESCRIPT/JAVASCRIPT
;;; ================================================================

(defun my/setup-typescript-development ()
  "Configure TypeScript/JavaScript development environment for current buffer."
  ;; Use Corfu with Cape if available, otherwise fall back to Company
  (if (package-installed-p 'corfu)
      ;; Corfu setup
      (when (featurep 'cape)
        ;; Add yasnippet support via Cape
        (add-to-list 'completion-at-point-functions #'cape-yasnippet t))
    ;; Company fallback setup
    (my/company-backend-for-hook 'lsp-completion-mode-hook
                                 '((company-capf :with company-yasnippet)
                                   company-dabbrev-code)))
  
  (lsp-deferred)
  (yas-minor-mode 1)
  (when (fboundp 'dap-mode)
    (dap-mode 1)))

(add-hook 'typescript-ts-mode-hook #'my/setup-typescript-development)
(add-hook 'tsx-ts-mode-hook #'my/setup-typescript-development)
(add-hook 'typescript-mode-hook #'my/setup-typescript-development)

;;; ================================================================
;;; LANGUAGE SUPPORT - OCAML
;;; ================================================================

(add-to-list 'load-path "/Users/jth/.opam/default/share/emacs/site-lisp")
(require 'ocp-indent)

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

;; OPAM (OCaml Package Manager) integration
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")

;; Development testing utilities
(load-file "~/.emacs.d/test-lsp-booster.el")
(load-file "~/.emacs.d/test-corfu-migration.el")
(load-file "~/.emacs.d/test-apheleia.el")
