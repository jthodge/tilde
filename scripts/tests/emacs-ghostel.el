;;; emacs-ghostel.el --- ERT tests for optional Ghostel integration -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'project)

(defconst tilde-ghostel--repo-root
  (expand-file-name "../.."
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defconst tilde-ghostel--modules-dir
  (expand-file-name "emacs/.emacs.d/modules" tilde-ghostel--repo-root))

(add-to-list 'load-path tilde-ghostel--modules-dir)

(defun tilde-ghostel--load ()
  (load (expand-file-name "ghostel-terminal.el" tilde-ghostel--modules-dir)
        nil t t))

(ert-deftest tilde-ghostel/project-command-errors-until-package-installed ()
  (cl-letf (((symbol-function 'package-installed-p) (lambda (&rest _) nil)))
    (tilde-ghostel--load)
    (should (eq (lookup-key project-prefix-map (kbd "T"))
                'my/ghostel-project))
    (should-error (my/ghostel-project) :type 'user-error)))

(ert-deftest tilde-ghostel/project-command-loads-package-in-project-root ()
  (let* ((home (file-name-as-directory (make-temp-file "tilde-ghostel-home-" t)))
         (pkg-dir (file-name-as-directory (make-temp-file "tilde-ghostel-pkg-" t)))
         (project-root-dir (file-name-as-directory
                            (make-temp-file "tilde-ghostel-project-" t)))
         (load-path (cons pkg-dir load-path))
         (user-emacs-directory (expand-file-name ".emacs.d/" home)))
    (make-directory user-emacs-directory t)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "ghostel.el" pkg-dir)
            (insert "(defvar ghostel-module-directory nil)\n"
                    "(defvar ghostel-kill-buffer-on-exit nil)\n"
                    "(defvar ghostel-query-before-killing t)\n"
                    "(defvar ghostel-term nil)\n"
                    "(defvar ghostel-semi-char-mode-map (make-sparse-keymap))\n"
                    "(defface ghostel-color-black '((t nil)) \"\")\n"
                    "(defvar ghostel-test-default-directory nil)\n"
                    "(defun ghostel () (setq ghostel-test-default-directory default-directory))\n"
                    "(provide 'ghostel)\n"))
          (cl-letf (((symbol-function 'package-installed-p)
                     (lambda (pkg) (eq pkg 'ghostel)))
                    ((symbol-function 'project-current)
                     (lambda (&optional _maybe-prompt) 'fake-project))
                    ((symbol-function 'project-root)
                     (lambda (_project) project-root-dir)))
            (tilde-ghostel--load)
            (my/ghostel-project)
            (should (equal ghostel-test-default-directory project-root-dir))
            (should (equal ghostel-module-directory
                           (expand-file-name "ghostel/" user-emacs-directory)))
            (should ghostel-kill-buffer-on-exit)
            (should-not ghostel-query-before-killing)
            (should (equal ghostel-term "xterm-ghostty"))
            (should (eq (lookup-key ghostel-semi-char-mode-map (kbd "C-g"))
                        'keyboard-quit))))
      (dolist (feature '(ghostel ghostel-terminal))
        (when (featurep feature) (unload-feature feature t)))
      (when (file-directory-p home) (delete-directory home t))
      (when (file-directory-p pkg-dir) (delete-directory pkg-dir t))
      (when (file-directory-p project-root-dir)
        (delete-directory project-root-dir t)))))
