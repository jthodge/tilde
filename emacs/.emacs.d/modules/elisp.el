;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; LANGUAGE SUPPORT - ELISP
;;; ================================================================

(defun my/setup-elisp-development ()
  "Configure Elisp development environment for current buffer."
  ;; Enable minor modes
  (yas-minor-mode 1)

  ;; Enable linting with flycheck and package-lint
  (when (package-installed-p 'flycheck)
    (require 'flycheck nil t)
    (flycheck-mode 1)

    ;; Enable package-lint if available
    (when (package-installed-p 'flycheck-package)
      (require 'flycheck-package nil t)
      (flycheck-package-setup)))

  ;; No LSP needed - Emacs provides all completion/navigation natively
  ;; Enhanced completion with Cape
  (when (package-installed-p 'cape)
    (require 'cape nil t)
    ;; Use built-in elisp completion as primary
    (setq-local completion-at-point-functions
                (list #'elisp-completion-at-point))
    ;; Add Cape enhancements
    (when (fboundp 'cape-yasnippet)
      (add-to-list 'completion-at-point-functions #'cape-yasnippet t))
    (when (fboundp 'cape-dabbrev)
      (add-to-list 'completion-at-point-functions #'cape-dabbrev t)))

  ;; Enable checkdoc for documentation linting
  (when (package-installed-p 'flycheck)
    (add-hook 'flycheck-mode-hook
              (lambda ()
                (when (eq major-mode 'emacs-lisp-mode)
                  (setq-local flycheck-checkers
                              (append flycheck-checkers '(emacs-lisp-checkdoc))))))))

(add-hook 'emacs-lisp-mode-hook #'my/setup-elisp-development)
