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
(load-theme 'ujelly)

;; Configure Font
(when (member "Berkeley Mono" (font-family-list))
  (set-frame-font "Berkeley Mono" t t))

;; Minibuffer Completion
(ido-mode 1)
(ido-everywhere)
(setq ido-enable-flex-matching t)
(fido-mode)

;; Handling Whitespace
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Create POSIX-defined Line on Save
;; https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_206
(setq-default require-final-newline t)

;; Single-spaced Sentences
(setq sentence-end-double-space nil)

;; Spaced Indentation
(setq-default indent-tabs-mode nil)

;; Tab Stop Distance
(setq-default tab-width 4)

;; Language-specific Indentation
(setq c-basic-offset 4)
(setq js-indent-level 2)
(setq css-indent-offset 2)

;; Highlight Parentheses
(setq show-paren-delay 0)
(show-paren-mode)

;; Separate Working Directory and Autosave, Backup Directories
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

;; Copy Duplicate Backup File
;; Default behavior: move, not copy
(setq backup-by-copying t)

;; Disable Lockfiles
(setq create-lockfiles nil)

;; Separate Third-Party Customization
;; Default behavior: automatically write to init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Enable  MELPA Installation
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install packages
(dolist (package '(markdown-mode paredit rainbow-delimiters))
  (unless (package-installed-p package)
    (package-install package)))

;; Add Paredit Hooks
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)

;; Add Rainbow Delimiters Hooks
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

;; Confiure Rainbow Delimiters Theme
(require 'rainbow-delimiters)
(set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
(set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
(set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
(set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
(set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
(set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
(set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
(set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray

;; Custom Commands
(defun show-current-time ()
  "Show current time."
  (interactive)
  (message (current-time-string)))

;; Custom Key Sequences
(global-set-key (kbd "C-c t") 'show-current-time)
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)

;; Spin Up Server
(require 'server)
(unless (server-running-p)
  (server-start))
