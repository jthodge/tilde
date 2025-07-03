;; init.el  -*- lexical-binding: t; -*-

;;; ================================================================
;;; BOOTSTRAP
;;; ================================================================

;; Suppress obsolete warnings from package dependencies
;; N.B. Upstream `cl` is the offending dependency package
(setq byte-compile-warnings '(not obsolete))

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
  '(company                ; Code and text completion framework
    consult                ; Incremental narrowing
    consult-lsp            ; Improve working between `consult` and `lsp-mode`
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
          (package-install package)))
      my-packages)

;;; ================================================================
;;; DEVELOPMENT TOOLS - GENERAL
;;; ================================================================

;; Company (text completion)
(require 'company)
(setopt company-idle-delay 0.1
        company-tooltip-align-annotations t)

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
        lsp-diagnostics-provider :flycheck)

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
    (let ((grammars '((javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
                      (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                      (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                      (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")))))
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
  (my/company-backend-for-hook 'lsp-completion-mode-hook
                               '((company-capf :with company-yasnippet)
                                 company-dabbrev-code))
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
  (my/company-backend-for-hook 'lsp-completion-mode-hook
                               '((company-capf :with company-yasnippet)
                                 company-dabbrev-code))
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
 '(package-selected-packages
   '(company consult-lsp dap-mode elfeed flycheck lsp-pyright lsp-ui
             typescript-mode yasnippet)))

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
