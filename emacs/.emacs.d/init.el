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

(defun uv-activate ()
  "Activate Python environment managed by uv based on current project directory.
Looks for .venv directory in project root and activates the Python interpreter.
Falls back to $HOME/.venv if no project-specific environment is found."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (project-venv-path (expand-file-name ".venv" project-root))
         (home-dir (getenv "HOME"))
         (home-venv-path (expand-file-name ".venv" home-dir))
         ;; Check for various possible Python executable locations
         (possible-python-paths '("bin/python" "bin/python3"
                                 "base/bin/python" "base/bin/python3"))
         (find-python-in-venv (lambda (venv-path)
                                (when (file-directory-p venv-path)
                                  (seq-find (lambda (exec-path)
                                             (let ((full-path (expand-file-name exec-path venv-path)))
                                               (file-exists-p full-path)))
                                           possible-python-paths))))
         (project-python-rel-path (and (file-directory-p project-venv-path)
                                      (funcall find-python-in-venv project-venv-path)))
         (home-python-rel-path (and (file-directory-p home-venv-path)
                                   (funcall find-python-in-venv home-venv-path)))
         ;; Select venv path and python path
         (selected-venv-path (cond
                              ((and project-python-rel-path) project-venv-path)
                              ((and home-python-rel-path) home-venv-path)
                              (t nil)))
         (selected-python-path (cond
                                ((and project-python-rel-path)
                                 (expand-file-name project-python-rel-path project-venv-path))
                                ((and home-python-rel-path)
                                 (expand-file-name home-python-rel-path home-venv-path))
                                (t nil))))

    ;; Debug output
    (message "Project venv exists: %s" (file-directory-p project-venv-path))
    (message "Home venv exists: %s" (file-directory-p home-venv-path))
    (message "Project python path: %s"
             (if project-python-rel-path
                 (expand-file-name project-python-rel-path project-venv-path)
               "not found"))
    (message "Home python path: %s"
             (if home-python-rel-path
                 (expand-file-name home-python-rel-path home-venv-path)
               "not found"))

    (if (and selected-venv-path selected-python-path)
        (progn
          ;; Set Python interpreter path
          (setq python-shell-interpreter selected-python-path)

          ;; Update exec-path to include the venv's bin directory
          (let ((venv-bin-dir (file-name-directory selected-python-path)))
            (setq exec-path (cons venv-bin-dir
                                  (remove venv-bin-dir exec-path))))

          ;; Update PATH environment variable
          (setenv "PATH" (concat (file-name-directory selected-python-path)
                                 path-separator
                                 (getenv "PATH")))

          ;; Update VIRTUAL_ENV environment variable
          (setenv "VIRTUAL_ENV" selected-venv-path)

          ;; Remove PYTHONHOME if it exists
          (setenv "PYTHONHOME" nil)

          (message "Activated UV Python environment at %s (using %s)"
                   selected-venv-path
                   selected-python-path))
      (error "No Python interpreter found in %s or %s venv directories" project-root home-dir))))

(defun uv-deactivate ()
  "Deactivate the current Python virtual environment.
If a project-specific environment is active, deactivate it and
fall back to $HOME/.venv if it exists.
Otherwise, perform default deactivation behavior."
  (interactive)
  (let* ((current-virtual-env (getenv "VIRTUAL_ENV"))
         (home-dir (getenv "HOME"))
         (home-venv-path (expand-file-name ".venv" home-dir))
         (home-venv-exists (file-directory-p home-venv-path))
         ;; Possible Python paths in home venv
         (possible-python-paths '("bin/python" "bin/python3"
                                 "base/bin/python" "base/bin/python3"))
         (find-python-in-venv (lambda (venv-path)
                                (when (file-directory-p venv-path)
                                  (seq-find (lambda (exec-path)
                                             (let ((full-path (expand-file-name exec-path venv-path)))
                                               (file-exists-p full-path)))
                                           possible-python-paths))))
         (home-python-rel-path (and home-venv-exists
                                   (funcall find-python-in-venv home-venv-path)))
         (home-python-path (when home-python-rel-path
                             (expand-file-name home-python-rel-path home-venv-path))))

    ;; First deactivate current environment if it exists
    (when current-virtual-env
      ;; Remove the virtual env's bin directories from exec-path
      (when-let* ((bin-dir (expand-file-name "bin" current-virtual-env)))
        (setq exec-path (remove bin-dir exec-path)))
      (when-let* ((base-bin-dir (expand-file-name "base/bin" current-virtual-env)))
        (setq exec-path (remove base-bin-dir exec-path)))

      ;; Remove the bin directories from PATH environment variable
      (let ((path-elements (split-string (getenv "PATH") path-separator))
            (bin-dir (expand-file-name "bin" current-virtual-env))
            (base-bin-dir (expand-file-name "base/bin" current-virtual-env)))
        (setenv "PATH"
                (mapconcat 'identity
                           (seq-filter (lambda (path)
                                         (not (or (string= path bin-dir)
                                                  (string= path base-bin-dir))))
                                      path-elements)
                           path-separator)))

      ;; Reset Python interpreter to the system default temporarily
      (setq python-shell-interpreter "python")

      ;; Unset VIRTUAL_ENV environment variable
      (setenv "VIRTUAL_ENV" nil)

      (message "Deactivated virtual environment: %s" current-virtual-env))

    ;; Now activate home venv if it exists and has a Python interpreter
    (if (and home-venv-exists home-python-path)
        (progn
          ;; Set Python interpreter path
          (setq python-shell-interpreter home-python-path)

          ;; Update exec-path to include the home venv's bin directory
          (let ((venv-bin-dir (file-name-directory home-python-path)))
            (setq exec-path (cons venv-bin-dir
                                  (remove venv-bin-dir exec-path))))

          ;; Update PATH environment variable
          (setenv "PATH" (concat (file-name-directory home-python-path)
                                 path-separator
                                 (getenv "PATH")))

          ;; Update VIRTUAL_ENV environment variable
          (setenv "VIRTUAL_ENV" home-venv-path)

          ;; Remove PYTHONHOME if it exists
          (setenv "PYTHONHOME" nil)

          (message "Activated fallback UV Python environment at %s" home-venv-path))

      ;; No home venv or no Python in home venv
      (unless current-virtual-env  ; Only show if we weren't deactivating something
        (message "No active virtual environment to deactivate."))))

  ;; Return nil to indicate function completed
  nil)

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

