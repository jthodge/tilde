;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; CORE UI CONFIGURATION
;;; ================================================================

;; UI cleanup
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

;; Minibuffer completion
(when (package-installed-p 'vertico)
  (with-eval-after-load 'vertico
    (vertico-mode))
  (require 'vertico nil t))

(when (package-installed-p 'orderless)
  (require 'orderless nil t)
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

(when (package-installed-p 'marginalia)
  (with-eval-after-load 'marginalia
    (marginalia-mode))
  (require 'marginalia nil t))
