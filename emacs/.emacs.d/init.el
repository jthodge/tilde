;; init.el

;; Configure UI
(menu-bar-mode 0)
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(setq inhibit-startup-screen t)
(column-number-mode)
(global-display-line-numbers-mode)

;; Configure Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'ujelly t)

;; Configure Font
(when (member "Berkeley Mono" (font-family-list))
  (set-frame-font "Berkeley Mono" t t))

;; Minibuffer Completion
(ido-mode 1)
(ido-everywhere)
(setq ido-enable-flex-matching t)
(fido-mode)

;; Handle Whitespace
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines 1)
(setq-default indicate-buffer-boundaries 'left)

;; Single-spaced Sentences
(setq sentence-end-double-space nil)

;; Spaced Indentation
(setq-default indent-tabs-mode nil)

;; Tab Stop Distance
(setq-default tab-width 4)

;; Create POSIX-defined Line on Save
;; https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_206
(setq-default require-final-newline t)

;; Highlight Parentheses
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Package Support
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
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
    which-key              ; Display currently available keybindings
    yasnippet              ; Snippet and template management
    ))

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      my-packages)

;; Set standard action keymaps
;; N.B. These prevent conflicts with `lsp-mode` keymaps
(global-set-key (kbd "C-c o l") #'org-store-link)
(global-set-key (kbd "C-c o b") #'org-switchb)

;; Text Completion
(require 'company)
(setopt company-idle-delay 0.1)
(setopt company-tooltip-align-annotations t)
(defmacro company-backend-for-hook (hook backends)
  `(add-hook ,hook (lambda ()
                     (set (make-local-variable 'company-backends)
                          ,backends))))

;; Snippets and template expansion
(with-eval-after-load 'yasnippet
  (yas-reload-all))

;; LSP
;; Set `lsp-mode` keymap prefix
(setopt lsp-keymap-prefix "C-c l")
;; Shut down LSP server when all buffers associated with the server are closed
(setopt lsp-keep-workspace-alive nil)
;; Configure breadcrumbs
(setopt lsp-headerline-breadcrumb-segments
        '(path-up-to-project
          file
          symbols))
;; Set delay for `lsp-ui` doc overlay
(setopt lsp-ui-doc-delay 0.5)
;; Set `lsp-mode` to use `flycheck` (instead of `flymake`) as diagnostics provider
(setopt lsp-diagnostics-provider :flycheck)

(with-eval-after-load 'lsp-ui
  ;; Map `xref-find-definitions` (Default: `M-.`)
  (define-key lsp-ui-mode-map
              [remap xref-find-definitions]
              #'lsp-ui-peek-find-definitions)

  ;; Map `xref-find-references` (Default: `M-?`)
  (define-key lsp-ui-mode-map
              [remap xref-find-references]
              #'lsp-ui-peek-find-references))

;; Configure `lsp-mode` enhancements
(with-eval-after-load 'lsp-mode
  ;; Remap `lsp-treemacs-errors-list` to `C-c l g e`
  (define-key lsp-mode-map
              [remap lsp-treemacs-errors-list]
              #'consult-lsp-diagnostics)
  ;; Remap `xref-find-apropos` to `C-c l g a`
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
  ;; Enable `which-key-mode` `lsp-mode` integration
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; Configure debugging
(setopt dap-auto-configure-mode t)

(defmacro company-backend-for-hook (hook backends)
  `(add-hook ,hook (lambda ()
                     (set (make-local-variable 'company-backends)
                          ,backends))))

;; Python
;; Load `python` mode when openin Python files
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; Set indentation
(setq python-indent-offset 4)

(defun set-up-python-env ()
  "Set up Python development environment in current buffer."
  (company-backend-for-hook 'lsp-completion-mode-hook
                        '((company-capf :with company-yasnippet)
                          company-dabbrev-code))

  ;; Enable LSP support in Python buffers
  (require 'lsp-pyright)

  ;; Start LSP server when switching to buffer
  (lsp-deferred)

  ;; Enable yasnippet minor mode only in python-mode
  (yas-minor-mode 1)

  ;; Find and set appropriate Python executable
  (setq-local python-shell-interpreter (executable-find "python"))

  ;; Enable DAP support in Python buffers
  (require 'dap-python)
  (setq-local dap-python-debugger 'debugpy)
  (dap-mode 1))

;; Configure hooks after `python-mode` is loaded
(add-hook 'python-mode-hook #'set-up-python-env)

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

