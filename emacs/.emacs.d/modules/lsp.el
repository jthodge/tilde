;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; PERFORMANCE OPTIMIZATIONS
;;; ================================================================

;; Increase amount of data Emacs reads from processes
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
;; Current lsp-mode may have changed plist handling...
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
        lsp-completion-show-kind t
        ;; Disable volar and other Vue servers for TypeScript files
        lsp-disabled-clients '(volar volar-api volar-doc volar-html vue-semantic-server))

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

  ;; Define missing lsp-flycheck faces and error levels
  (unless (facep 'lsp-flycheck-error-unnecessary)
    (defface lsp-flycheck-error-unnecessary
      '((t :strike-through t :inherit font-lock-comment-face))
      "Face for unnecessary code."
      :group 'lsp-faces))

  ;; Register the error level with flycheck when it loads
  (with-eval-after-load 'flycheck
    (when (boundp 'flycheck-error-levels)
      (unless (assq 'lsp-flycheck-error-unnecessary flycheck-error-levels)
        (flycheck-define-error-level 'lsp-flycheck-error-unnecessary
          :severity 'warning
          :compilation-level 1
          :overlay-category 'flycheck-warning-overlay
          :fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
          :fringe-face 'flycheck-fringe-warning
          :error-list-face 'flycheck-error-list-warning))))

  ;; File watching ignore patterns
  (setq lsp-file-watch-ignored-directories
        (append lsp-file-watch-ignored-directories
                '("[/\\\\]\\.Trash\\'"
                  "[/\\\\]\\.git\\'"
                  "[/\\\\]\\.venv\\'"
                  "[/\\\\]__pycache__\\'"
                  "[/\\\\]node_modules\\'"
                  "[/\\\\]\\.DS_Store\\'")))

  ;; Consult integration
  (define-key lsp-mode-map [remap lsp-treemacs-errors-list] #'consult-lsp-diagnostics)
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)

  ;; Which-key integration
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;;; ----------------------------------------------------------------
;;; Language-Specific LSP Servers
;;; ----------------------------------------------------------------

;; Configure TypeScript language server and silence volar
(with-eval-after-load 'lsp-mode
  ;; Silence all volar messages
  (defun lsp-volar--activate-p (&rest _)
    "Override volar activation to always return nil."
    nil)

  ;; Ensure volar is never activated
  (with-eval-after-load 'lsp-volar
    (setq lsp-volar-take-over-mode nil)
    (setq lsp-volar-hybrid-mode nil))

  ;; Disable company-mode warnings
  (setq lsp-completion-provider :none))

;; Configure Go language server (gopls)
(with-eval-after-load 'lsp-mode
  ;; Configure gopls settings for single-module projects with future multi-module support
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "gopls")
    :activation-fn (lsp-activate-on "go")
    :server-id 'gopls
    :initialization-options
    '(:buildFlags []
                  :env nil
                  :directoryFilters []
                  :templateExtensions []
                  :memoryMode ""
                  :analyses (:fieldalignment t
                                             :nilness t
                                             :unusedparams t
                                             :unusedwrite t)
                  :staticcheck t
                  :codelenses (:gc_details t
                                           :generate t
                                           :regenerate_cgo t
                                           :test t
                                           :tidy t
                                           :upgrade_dependency t
                                           :vendor t))))

  ;; Configure Go imports and formatting hooks
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))
