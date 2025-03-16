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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f747c4004e38bcdc131649a90325c00d246bb7dc73bc6ab6e0e7ab5489da8459"
     "a4340c197a450c77c729cad236b5f3ca88aaf974e91a7af2d2e7ae7bb5f96720"
     "6b20d669fcbcd79c6d0f3db36a71af1b88763246d3550a0c361866adecb38a9e"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

