;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; PACKAGE MANAGEMENT
;;; ================================================================
;;
;; Package archives are declared so `package-install` works, but no
;; refresh and no install runs at startup. That keeps `emacs -Q` and
;; batch tests offline and deterministic.
;;
;; To install or update the declared set interactively, run
;;   M-x my/install-packages
;; which refreshes archives once and then installs only what is
;; missing. Missing tools produce a clear message; nothing is fetched
;; automatically.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defconst my-packages
  '(apheleia               ; Asynchronous code formatting
    cape                   ; Completion At Point Extensions for Corfu
    consult                ; Incremental narrowing
    consult-lsp            ; Improve working between `consult` and `lsp-mode`
    corfu                  ; Modern completion frontend
    dap-mode               ; Debug Adapter Protocol Support
    embark                 ; Contextual actions (C-. / C-;)
    embark-consult         ; Consult integration for Embark
    exec-path-from-shell   ; Import PATH/CPATH/LIBRARY_PATH in GUI Emacs
    flycheck               ; Linting and syntax checker
    flycheck-package       ; Elisp package linting for MELPA standards
    go-mode                ; Go editing support
    lsp-mode               ; Language Server Protocol support
    lsp-pyright            ; Language Server Protocol client using pyright Python Language Server
    lsp-ui                 ; UI improvements for `lsp-mode`
    magit                  ; Git porcelain (autoloaded via C-c g)
    marginalia             ; Rich completion annotations
    orderless              ; Flexible completion matching
    vertico                ; Modern vertical completion
    which-key              ; Display currently available keybindings
    yasnippet              ; Snippet and template management
    )
  "Packages required by this configuration.
Nothing here is installed at startup. Use `my/install-packages'.")

(defun my/install-packages (&optional refresh)
  "Install any declared package that is not yet present.
With prefix arg REFRESH, refresh archive contents first.
Failures are reported and do not abort the rest of the run."
  (interactive "P")
  (when (or refresh (null package-archive-contents))
    (package-refresh-contents))
  (let ((missing (seq-remove #'package-installed-p my-packages))
        (failed '()))
    (if (null missing)
        (message "All declared packages already installed.")
      (message "Installing %d package(s): %s"
               (length missing)
               (mapconcat #'symbol-name missing " "))
      (dolist (pkg missing)
        (condition-case err
            (package-install pkg)
          (error
           (push (cons pkg err) failed)
           (message "Failed to install package %s: %s" pkg err))))
      (if failed
          (message "Finished with %d failure(s): %s"
                   (length failed)
                   (mapconcat (lambda (cell) (symbol-name (car cell)))
                              failed " "))
        (message "Installed %d package(s)." (length missing))))))

;; Report declared-but-missing packages with a plain message. No
;; network is contacted; individual modules already guard on
;; `package-installed-p' before using their optional dependencies.
(let ((missing (seq-remove #'package-installed-p my-packages)))
  (when missing
    (message "Declared packages not installed (run M-x my/install-packages): %s"
             (mapconcat #'symbol-name missing " "))))
