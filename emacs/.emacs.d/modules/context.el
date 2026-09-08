;;; context.el --- Persistent editing context -*- lexical-binding: t; -*-

;; Built-in histories, recent files, refresh and project commands. State is
;; private and ignored; names are qualified so same-named projects do not
;; accidentally share a shell. This is not a custom terminal implementation.
(require 'savehist)
(require 'recentf)
(require 'autorevert)
(require 'project)
(require 'proj-context)

(defvar my/context--state-dir
  (expand-file-name "state/" user-emacs-directory)
  "Directory for private local context state.")
(defvar my/context--history-file
  (expand-file-name "history" my/context--state-dir))
(defvar my/context--recentf-file
  (expand-file-name "recentf" my/context--state-dir))

(defun my/context--ensure-private (path)
  "Create PATH if missing, without truncation; restrict its permissions."
  (make-directory (file-name-directory path) t)
  (set-file-modes (file-name-directory path) #o700)
  (unless (file-exists-p path)
    (with-temp-file path (insert "")))
  (set-file-modes path #o600))

(setq savehist-file my/context--history-file
      savehist-file-modes #o600
      history-length 200
      savehist-save-minibuffer-history t
      savehist-additional-variables
      '(search-ring regexp-search-ring extended-command-history))
;; Exclude obvious sensitive histories. Other histories can still contain
;; private input: these files are local state, not suitable for publication.
(dolist (sym '(kill-ring register-alist shell-command-history command-history
               read-expression-history eval-expression-history))
  (add-to-list 'savehist-ignored-variables sym))
(my/context--ensure-private savehist-file)
(unless savehist-mode (savehist-mode 1))

(setq recentf-save-file my/context--recentf-file
      recentf-save-file-modes #o600
      recentf-max-saved-items 200
      ;; Do not clean old remote entries by contacting hosts at startup.
      recentf-auto-cleanup 'never)
(my/context--ensure-private recentf-save-file)
(unless recentf-mode (recentf-mode 1))

(setq auto-revert-remote-files nil
      global-auto-revert-non-file-buffers nil
      auto-revert-verbose nil)
;; Built-in refresh leaves modified buffers alone.
(unless global-auto-revert-mode (global-auto-revert-mode 1))

(defun my/context--qualify-project-buffer (name)
  "Qualify project-prefixed NAME with its canonical root.
The built-in basename alone collides for separate repositories named alike."
  (if-let* ((proj (project-current nil)))
      (format "%s<%s>" name
              (abbreviate-file-name
               (directory-file-name (my/proj--norm (project-root proj)))))
    name))
;; Keep the built-in shell/eshell implementation and project labels. Advice
;; with the same named function replaces itself on reload rather than stacking.
(advice-add 'project-prefixed-buffer-name :filter-return
            #'my/context--qualify-project-buffer)
(setq project-compilation-buffer-name-function #'project-prefixed-buffer-name)

;; Existing C-c p b/s/c retain project buffer/shell/compile commands.
(unless (global-key-binding (kbd "C-c r"))
  (global-set-key (kbd "C-c r") #'recentf-open))

(provide 'context)
;;; context.el ends here
