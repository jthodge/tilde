;;; -*- lexical-binding: t; -*-
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
    flycheck-package       ; Elisp package linting for MELPA standards
    go-mode                ; Go editing support
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
