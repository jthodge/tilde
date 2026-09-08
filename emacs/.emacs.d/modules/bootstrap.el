;;; -*- lexical-binding: t; -*-
;; init.el  -*- lexical-binding: t; -*-

;;; ================================================================
;;; BOOTSTRAP
;;; ================================================================

;; Suppress obsolete warnings from package dependencies
;; N.B. Upstream `cl` is offending dependency package
(setq byte-compile-warnings '(not obsolete))

;; TODO: Add cargo bin to exec-path for tools e.g. emacs-lsp-booster
;; (let ((cargo-bin (expand-file-name "~/.cargo/bin")))
;;   (when (file-directory-p cargo-bin)
;;     (add-to-list 'exec-path cargo-bin)))

;; Add Go bin to exec-path for development tools
(let ((go-bin (expand-file-name "~/go/bin")))
  (when (file-directory-p go-bin)
    (add-to-list 'exec-path go-bin)
    ;; Also update PATH environment variable for subprocesses
    (setenv "PATH" (concat go-bin path-separator (getenv "PATH")))))

(load "~/.emacs.d/scripts.el")
