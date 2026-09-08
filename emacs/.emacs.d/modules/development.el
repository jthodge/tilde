;;; -*- lexical-binding: t; -*-
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

    ;; Configure Elisp formatting
    (setf (alist-get 'emacs-lisp-mode apheleia-mode-alist) 'lisp-indent)

    ;; Configure Go formatter
    (setf (alist-get 'gofmt apheleia-formatters)
          '("gofmt"))

    ;; Configure mode associations for Go files
    (setf (alist-get 'go-mode apheleia-mode-alist) 'gofmt)

    ;; Configure TypeScript/JavaScript formatters with Prettier
    ;; Use yarn to run prettier from project's node_modules
    (setf (alist-get 'prettier apheleia-formatters)
          '("yarn" "prettier" "--stdin-filepath" filepath))

    ;; Configure mode associations for TypeScript/JavaScript files
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
