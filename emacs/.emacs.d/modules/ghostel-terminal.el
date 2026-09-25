;;; ghostel.el --- Optional Ghostty-backed project terminals -*- lexical-binding: t; -*-

(require 'project)

(defun my/ghostel-project ()
  "Open Ghostel in the current project root.

Ghostel is optional. This command gives a clear user error until the package is
installed with `my/install-packages'."
  (interactive)
  (unless (require 'ghostel nil t)
    (user-error "Ghostel is not installed; run M-x my/install-packages"))
  (let ((default-directory (project-root (project-current t))))
    (ghostel)))

(define-key project-prefix-map (kbd "T") #'my/ghostel-project)

(when (package-installed-p 'ghostel)
  (with-eval-after-load 'ghostel
    ;; Keep the native module outside elpa/ so package updates do not delete it
    ;; from under a running Emacs.
    (setq ghostel-module-directory (expand-file-name "ghostel/" user-emacs-directory)
          ghostel-kill-buffer-on-exit t
          ghostel-query-before-killing nil
          ghostel-term "xterm-ghostty")

    ;; Use a Ghostty-oriented terminal palette rather than Emacs's default
    ;; ansi-color faces. Guard face assignment so byte/batch tests do not need
    ;; Ghostel loaded from ELPA.
    (dolist (face-spec '((ghostel-color-black . "#626262")
                         (ghostel-color-red . "#ff8373")
                         (ghostel-color-green . "#b4fb73")
                         (ghostel-color-yellow . "#fffdc3")
                         (ghostel-color-blue . "#a5d5fe")
                         (ghostel-color-magenta . "#ff90fe")
                         (ghostel-color-cyan . "#d1d1fe")
                         (ghostel-color-white . "#f1f1f1")
                         (ghostel-color-bright-black . "#8f8f8f")
                         (ghostel-color-bright-red . "#ffc4be")
                         (ghostel-color-bright-green . "#d6fcba")
                         (ghostel-color-bright-yellow . "#fffed5")
                         (ghostel-color-bright-blue . "#c2e3ff")
                         (ghostel-color-bright-magenta . "#ffb2fe")
                         (ghostel-color-bright-cyan . "#e6e6fe")
                         (ghostel-color-bright-white . "#ffffff")))
      (when (facep (car face-spec))
        (set-face-attribute (car face-spec) nil :foreground (cdr face-spec))))

    (when (boundp 'ghostel-semi-char-mode-map)
      (define-key ghostel-semi-char-mode-map (kbd "C-g") #'keyboard-quit)
      (define-key ghostel-semi-char-mode-map (kbd "<home>") #'beginning-of-buffer)
      (define-key ghostel-semi-char-mode-map (kbd "<end>") #'end-of-buffer))))

(provide 'ghostel-terminal)
