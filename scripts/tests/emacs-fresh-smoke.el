;;; emacs-fresh-smoke.el --- Full init in a fresh HOME with no packages -*- lexical-binding: t; -*-
;;
;; Complements `emacs-smoke.el', which loads the installed configuration
;; against the developer's real `package-user-dir'. This suite instead:
;;
;;   * points `package-user-dir' at an empty temporary directory, so no
;;     third-party package is available at load time;
;;   * blocks the network and any implicit install with `cl-letf'
;;     overrides that raise on call;
;;   * loads the tracked `init.el' verbatim;
;;   * opens a sample source buffer for each language module we ship
;;     (Python, Emacs Lisp, Go, TypeScript) and confirms `find-file'
;;     does not error.
;;
;; The mocks intentionally do not stub `yas-minor-mode' or `lsp-deferred'.
;; The language modules must guard those calls themselves.
;;
;; Run:
;;   emacs -Q --batch -l scripts/tests/emacs-fresh-smoke.el

(require 'cl-lib)
(require 'package)

(defun tilde-fresh-smoke--set-mode (buffer)
  "Explicitly rerun `set-auto-mode' on BUFFER.
`find-file-noselect' already calls `after-find-file' -> `normal-mode'
-> `set-auto-mode', but calling it again is cheap and makes intent
obvious in the batch log."
  (with-current-buffer buffer
    (set-auto-mode)))

(let* ((repo (expand-file-name "../.." (file-name-directory load-file-name)))
       (source (expand-file-name "emacs/.emacs.d/" repo))
       (home (make-temp-file "tilde-emacs-fresh-" t))
       (user-emacs-directory (expand-file-name ".emacs.d/" home))
       (package-user-dir (expand-file-name "elpa/" user-emacs-directory))
       (process-environment (copy-sequence process-environment)))
  (unwind-protect
      (progn
        (setenv "HOME" home)
        (setenv "XDG_CONFIG_HOME" (expand-file-name ".config" home))
        (setenv "XDG_CACHE_HOME" (expand-file-name ".cache" home))
        (make-directory user-emacs-directory t)
        (make-directory package-user-dir t)
        ;; Link the tracked module tree into the fresh HOME so we load
        ;; the real source, not a copy that could drift.
        (dolist (name '("modules" "themes" "scripts.el"))
          (make-symbolic-link (expand-file-name name source)
                              (expand-file-name name user-emacs-directory)))
        (when (fboundp 'startup-redirect-eln-cache)
          (startup-redirect-eln-cache (expand-file-name "eln-cache/" home)))
        (cl-letf (((symbol-function 'package-refresh-contents)
                   (lambda (&rest _) (error "Unexpected package refresh")))
                  ((symbol-function 'package-install)
                   (lambda (&rest _) (error "Unexpected package install")))
                  ((symbol-function 'url-retrieve)
                   (lambda (&rest _) (error "Unexpected network request")))
                  ((symbol-function 'url-retrieve-synchronously)
                   (lambda (&rest _) (error "Unexpected network request"))))
          (load (expand-file-name "init.el" source) nil t t)
          ;; Confirm nothing left behind an active LSP or yasnippet.
          (when (fboundp 'lsp-mode)
            (error "lsp-mode became fboundp under a fresh HOME"))
          (when (fboundp 'yas-minor-mode)
            (error "yas-minor-mode became fboundp under a fresh HOME"))
          ;; Open one buffer per language module. Each `find-file' must
          ;; succeed without a redisplay-time signal.
          (let ((cases '(("sample.py" . "print('hi')\n")
                         ("sample.el" . "(defun x () 1)\n")
                         ("sample.go" . "package main\nfunc main() {}\n")
                         ("sample.ts" . "export const x = 1;\n")
                         ("sample.tsx" . "export const X = () => null;\n"))))
            (dolist (case cases)
              (let* ((file (expand-file-name (car case) home))
                     (body (cdr case))
                     (buf nil))
                (with-temp-file file (insert body))
                (setq buf (find-file-noselect file))
                (unwind-protect
                    (with-current-buffer buf
                      (tilde-fresh-smoke--set-mode buf)
                      (unless (derived-mode-p 'prog-mode)
                        (error "%s: expected a programming mode, got %S" file major-mode))
                      (goto-char (point-max))
                      (insert "\n")
                      (unless (buffer-modified-p)
                        (error "%s: editing failed" file))
                      (set-buffer-modified-p nil))
                  (when (buffer-live-p buf)
                    (kill-buffer buf)))))))
        (princ "PASS: fresh HOME init opens Python/Elisp/Go/TypeScript/TSX sources without installed packages\n"))
    (delete-directory home t)))
