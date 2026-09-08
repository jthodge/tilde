;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; TREE-SITTER CONFIGURATION
;;; ================================================================
;;
;; Grammars are only *registered* here. Nothing is downloaded or built
;; at startup; installation would need network and a working toolchain,
;; and doing it silently on Emacs launch has burned enough sessions.
;;
;; To install every declared grammar interactively, run
;;   M-x my/install-treesitter-grammars
;; which iterates `treesit-language-source-alist' and calls
;; `treesit-install-language-grammar' for each missing entry.
;;
;; Mode remaps and auto-mode-alist entries are added *only* when the
;; corresponding grammar is actually available in the running Emacs.
;; If a grammar is missing the file simply opens in the classic mode.

(when (fboundp 'treesit-available-p)
  (require 'treesit))

(defconst my/treesitter-language-sources
  '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
    (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
    (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
    (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
    (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
    (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
    (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")))
  "Declared tree-sitter grammar sources.
Registration is safe and offline; installation is explicit.")

(defconst my/treesitter-mode-remaps
  '((typescript typescript-mode . typescript-ts-mode)
    (javascript js-mode . js-ts-mode)
    (javascript js2-mode . js-ts-mode)
    (json json-mode . json-ts-mode)
    (json js-json-mode . json-ts-mode))
  "Major-mode remap table.
Each entry is (GRAMMAR OLD-MODE . NEW-MODE). The remap is applied
only when GRAMMAR is available; otherwise the original mode is
left untouched.")

(defconst my/treesitter-auto-modes
  '((tsx "\\.tsx\\'" . tsx-ts-mode)
    (typescript "\\.ts\\'" . typescript-ts-mode)
    (tsx "\\.jsx\\'" . tsx-ts-mode)
    (javascript "\\.js\\'" . js-ts-mode)
    (javascript "\\.mjs\\'" . js-ts-mode)
    (typescript "\\.mts\\'" . typescript-ts-mode)
    (javascript "\\.cjs\\'" . js-ts-mode)
    (json "\\.json\\'" . json-ts-mode))
  "File association table.
Each entry is (GRAMMAR REGEX . MODE). The association is added
only when GRAMMAR is available.")

(defun my/setup-treesitter-grammars ()
  "Register tree-sitter grammar *sources* only. No download."
  (when (boundp 'treesit-language-source-alist)
    (dolist (grammar my/treesitter-language-sources)
      (add-to-list 'treesit-language-source-alist grammar))))

(defun my/install-treesitter-grammars ()
  "Install every declared tree-sitter grammar that is missing.
Interactive only. Requires network and a C toolchain."
  (interactive)
  (unless (and (fboundp 'treesit-available-p) (treesit-available-p))
    (user-error "Tree-sitter is not available in this Emacs build"))
  (my/setup-treesitter-grammars)
  (dolist (grammar my/treesitter-language-sources)
    (let ((lang (car grammar)))
      (condition-case err
          (unless (treesit-language-available-p lang)
            (message "Installing tree-sitter grammar: %s" lang)
            (treesit-install-language-grammar lang))
        (error
         (message "Failed to install tree-sitter grammar for %s: %s" lang err))))))

(defun my/treesitter-grammar-available-p (lang)
  "Return non-nil if tree-sitter grammar LANG is available in this Emacs."
  (and (fboundp 'treesit-language-available-p)
       (treesit-language-available-p lang)))

(defun my/setup-treesitter-mode-remapping ()
  "Register mode remaps for grammars that are actually installed."
  (when (boundp 'major-mode-remap-alist)
    (dolist (entry my/treesitter-mode-remaps)
      (let ((lang (nth 0 entry))
            (mapping (cons (nth 1 entry) (cddr entry))))
        (when (my/treesitter-grammar-available-p lang)
          (add-to-list 'major-mode-remap-alist mapping))))))

(defun my/setup-treesitter-auto-modes ()
  "Register auto-mode entries for grammars that are actually installed."
  (dolist (entry my/treesitter-auto-modes)
    (let ((lang (nth 0 entry))
          (assoc (cons (nth 1 entry) (cddr entry))))
      (when (my/treesitter-grammar-available-p lang)
        (add-to-list 'auto-mode-alist assoc)))))

;; Register sources unconditionally so `my/install-treesitter-grammars'
;; has something to iterate. Actual remaps only happen when grammars
;; are already installed - no network, no build, no surprise.
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (my/setup-treesitter-grammars)
  (my/setup-treesitter-mode-remapping)
  (my/setup-treesitter-auto-modes))
