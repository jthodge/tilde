;; init.el

(require 'use-package)
(require 'quelpa-use-package)

;; Configure UI
(menu-bar-mode 0)
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(setq inhibit-startup-screen t)
(column-number-mode)
(global-display-line-numbers-mode)

;; Configure Theme
(load-theme 'ujelly)

;; Configure Font
(when (member "Berkeley Mono" (font-family-list))
  (set-frame-font "Berkeley Mono" t t))

;; Minibuffer Completion
(ido-mode 1)
(ido-everywhere)
(setq ido-enable-flex-matching t)
(fido-mode)

;; Handling Whitespace
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Create POSIX-defined Line on Save
;; https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_206
(setq-default require-final-newline t)

;; Single-spaced Sentences
(setq sentence-end-double-space nil)

;; Spaced Indentation
(setq-default indent-tabs-mode nil)

;; Tab Stop Distance
(setq-default tab-width 4)

;; Language-specific Indentation
(setq c-basic-offset 4)
(setq js-indent-level 2)
(setq css-indent-offset 2)

;; Highlight Parentheses
(setq show-paren-delay 0)
(show-paren-mode)

;; Separate Working Directory and Autosave, Backup Directories
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

;; Copy Duplicate Backup File
;; Default behavior: move, not copy
(setq backup-by-copying t)

;; Disable Lockfiles
(setq create-lockfiles nil)

;; Separate Third-Party Customization
;; Default behavior: automatically write to init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Enable  MELPA Installation
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure Environment Variable Parity Between Emacs and Shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Install packages
(dolist (package '(markdown-mode paredit rainbow-delimiters))
  (unless (package-installed-p package)
    (package-install package)))

;; Add Paredit Hooks
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
(defun override-slime-del-key ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-del-key)

;; Add Rainbow Delimiters Hooks
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)

;; Confiure Rainbow Delimiters Theme
(require 'rainbow-delimiters)
(set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
(set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
(set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
(set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
(set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
(set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
(set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
(set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray

;; Custom Commands
(defun show-current-time ()
  "Show current time."
  (interactive)
  (message (current-time-string)))

;; Custom Key Sequences
(global-set-key (kbd "C-c t") 'show-current-time)
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Spin Up Server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Configure SBCL as SLIME's Lisp Compiler
(add-to-list 'exec-path "/usr/local/bin")
(setq inferior-lisp-program "sbcl")

;; Congfigure tree-sitter for General Use
(use-package tree-sitter
             :ensure t
             :config
             ;; Activate tree-sitter on all buffers containing code it has a parser available for
             (global-tree-sitter-mode)
             ;; tree-sitter-hl-mode causes immediately observable improvements for py, ts, and tsx
             ;; Enable/disable flag to compare results:
             ;; M-x tree-sitter-hl-mode
             (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
             :ensure t
             :after tree-sitter)

;; Use tree-sitter Dedicated tsx Parser for .tsx Files
;; Default behavior: tree-sitter uses typescript parser, which does not parse .tsx
;; To do so, create a derived mode that maps to both .ts and .tsx
;; The TypeScript language server will select tsx support based on the derived mode's name
;; tree-sitter will select its tsx parser based on the explicit mapping
(use-package typescript-mode
  :after tree-sitter
  :config
  ;; Using this instead of tsx-mode to allows eglot to automatically determine and configure the server language
  ;; https://github.com/joaotavora/eglot/issues/624
  ;; https://github.com/joaotavora/eglot/pull/674
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; Use the derived mode for .tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; By default, typescript-mode is mapped to the tree-sitter typescript parser
  ;; To change that behavior, use the derived mode to map .tsx _and_ .ts to typescriptreact-mode to tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; Configure tsi.el to Provide tree-sitter Indentation for TypeScript/JavaScript/TSX/JSX, JSON, and S/CSS
;; https://github.com/orzechowskid/tsi.el/
(use-package tsi
  :after tree-sitter
  ;; tsi.el hasn't been release on melpa, yet
  :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;; Auto-format on Save
;; https://github.com/radian-software/apheleia
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

;; Configure eglot
;; Alternative to lsp-mode
(use-package eglot
  :ensure t)

;; Configure elpy
;; (elpy-enable)
(use-package elpy
  :ensure t
  :init
  (elpy-enable))
(pyenv-mode)
(setq python-shell-completion-native-enable nil)

;; (use-package elpy
;;     :init
;;     (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
;;     :bind (:map elpy-mode-map
;;           ("<M-left>" . nil)
;;           ("<M-right>" . nil)
;;           ("<M-S-left>" . elpy-nav-indent-shift-left)
;;           ("<M-S-right>" . elpy-nav-indent-shift-right)
;;           ("M-." . elpy-goto-definition)
;;           ("M-," . pop-tag-mark))
;;     :config
;;     (setq elpy-rpc-backend "jedi"))

;; (use-package python
;;   :mode ("\\.py" . python-mode)
;;   :config
;;   (setq python-indent-offset 4)
;;   (elpy-enable))

;; (use-package pyenv-mode
;;   :init
;;   (add-to-list 'exec-path "~/.pyenv/shims")
;;   (setenv "WORKON_HOME" "~/.pyenv/versions/")
;;   :config
;;   (pyenv-mode)
;;   :bind
;;   ("C-x p e" . pyenv-activate-current-project))

;; (defun pyenv-activate-current-project ()
;;   "Automatically activates pyenv version if .python-version file exists."
;;   (interactive)
;;   (f-traverse-upwards
;;    (lambda (path)
;;      (message path)
;;      (let ((pyenv-version-path (f-expand ".python-version" path)))
;;        (if (f-exists? pyenv-version-path)
;;             (let ((pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
;;               (pyenv-mode-set pyenv-current-version)
;;               (message (concat "Setting virtualenv to " pyenv-current-version))))))))

;; (defvar pyenv-current-version nil nil)

;; (defun pyenv-init()
;;   "Initialize pyenv's current version to the global one."
;;   (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
;;     (message (concat "Setting pyenv version to " global-pyenv))
;;     (pyenv-mode-set global-pyenv)
;;     (setq pyenv-current-version global-pyenv)))

;; (add-hook 'after-init-hook 'pyenv-init)

;; (setq elpy-rpc-virtualenv-path "/Users/th/.pyenv/versions/3.11.0a1/envs/elpy-rpc")
;; (setq elpy-rpc-virtualenv-path "/Users/th/tilde/emacs/elpy-epc-virtualenv")
;; (setq elpy-rpc-virtualenv-path 'current)

;; Configure Flycheck for On-the-fly Syntax Checking
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
