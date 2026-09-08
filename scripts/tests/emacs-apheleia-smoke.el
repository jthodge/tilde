;;; emacs-apheleia-smoke.el --- Installed-package Apheleia dispatch smoke -*- lexical-binding: t; -*-
;;
;; Loads the REAL installed Apheleia and drives its dispatch code path
;; against the configured `apheleia-formatters' / `apheleia-mode-alist'
;; entries produced by `modules/development.el'. No real formatter
;; runs: we call `apheleia--formatter-context' directly (the function
;; that resolves a formatter definition into an executable + argv
;; against the current buffer) and assert on its returned context
;; object. `apheleia--execute-formatter-process' is stubbed as a
;; belt-and-suspenders check that no subprocess would be spawned if
;; a higher-level dispatch path were reached.
;;
;; Rationale (round-2 review): the earlier `emacs-proj-context.el'
;; "dispatch" test hand-rolled the eval pass and never loaded
;; Apheleia; it could not detect a regression that changed how
;; Apheleia splices form results into argv. Calling the real
;; `apheleia--formatter-context' is the smallest change that turns
;; the assertion into an actual integration smoke.
;;
;; Runs only when the Apheleia package is present in the developer's
;; `package-user-dir'. Missing Apheleia prints SKIP and exits 0 so
;; `make smoke' still passes on a fresh machine.
;;
;; Run:
;;   emacs -Q --batch -l scripts/tests/emacs-apheleia-smoke.el

(require 'cl-lib)
(require 'package)

(defvar tilde-apheleia-smoke--repo-root
  (expand-file-name "../.."
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defvar tilde-apheleia-smoke--modules-dir
  (expand-file-name "emacs/.emacs.d/modules" tilde-apheleia-smoke--repo-root))

(defun tilde-apheleia-smoke--locate-apheleia ()
  "Return the loadable directory for Apheleia, or nil.
Prefers the user's real `package-user-dir' (populated by
`emacs-smoke.el' / `make smoke') so we exercise the same
version the interactive Emacs would load."
  (package-initialize)
  (let ((apheleia (car (alist-get 'apheleia package-alist))))
    (and apheleia (package-desc-dir apheleia))))

(defun tilde-apheleia-smoke--mkexe (path)
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert "#!/bin/sh\nexit 0\n"))
  (set-file-modes path #o755))

(defun tilde-apheleia-smoke--touch (path)
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert "")))

(defun tilde-apheleia-smoke--run ()
  (let ((apheleia-dir (tilde-apheleia-smoke--locate-apheleia)))
    (unless apheleia-dir
      (princ "SKIP: apheleia package not installed under package-user-dir\n")
      (throw 'tilde-apheleia-skip nil))
    (add-to-list 'load-path apheleia-dir)
    (add-to-list 'load-path tilde-apheleia-smoke--modules-dir)
    (require 'apheleia)
    (require 'apheleia-formatters)
    (load (expand-file-name "proj-context.el"
                            tilde-apheleia-smoke--modules-dir)
          nil t t)
    ;; Load the real development module, including its installed integrations.
    (load (expand-file-name "development.el"
                            tilde-apheleia-smoke--modules-dir)
          nil t t)
    ;; Confirm the tracked configuration actually populated the alist
    ;; entries we're about to exercise. These are the exact keys the
    ;; interactive Emacs uses; if they drift, the smoke fails loudly
    ;; rather than silently testing nothing.
    (cl-assert (equal (alist-get 'prettier apheleia-formatters)
                      '((my/apheleia-prettier-arg1)
                        "--stdin-filepath" filepath))
               nil "prettier alist entry drifted: %S"
               (alist-get 'prettier apheleia-formatters))
    (cl-assert (equal (alist-get 'my/go-format apheleia-formatters)
                      '((my/apheleia-go-arg1)))
               nil "my/go-format alist entry drifted: %S"
               (alist-get 'my/go-format apheleia-formatters))
    (cl-assert (eq (alist-get 'typescript-ts-mode apheleia-mode-alist)
                   'prettier)
               nil "typescript-ts-mode -> prettier mapping missing")
    (cl-assert (eq (alist-get 'go-mode apheleia-mode-alist)
                   'my/go-format)
               nil "go-mode -> my/go-format mapping missing")
    ;; Prove no subprocess would be spawned even if higher-level
    ;; dispatch were reached. `apheleia--formatter-context' is a
    ;; pure resolver so it never calls this, but the advice
    ;; documents intent and would fire on any accidental escalation.
    (defun tilde-apheleia-smoke--refuse-process (&rest _)
      (error "smoke: apheleia would have spawned a subprocess"))
    (advice-add 'apheleia--execute-formatter-process :override
                #'tilde-apheleia-smoke--refuse-process)
    (unwind-protect
        (let ((root (file-name-as-directory
                     (file-truename (make-temp-file "tilde-apheleia-smoke-" t)))))
          (unwind-protect
              (progn
                (tilde-apheleia-smoke--touch
                 (expand-file-name "package.json" root))
                (tilde-apheleia-smoke--touch
                 (expand-file-name "pnpm-lock.yaml" root))
                (tilde-apheleia-smoke--mkexe
                 (expand-file-name "node_modules/.bin/prettier" root))
                (let ((ts-file (expand-file-name "src/a.ts" root)))
                  (tilde-apheleia-smoke--touch ts-file)
                  ;; Pin vcs-root so the temp dir (outside any real
                  ;; git repo) still terminates the upward walk.
                  (cl-letf (((symbol-function 'my/proj--vcs-root)
                             (lambda (&rest _) root)))
                    (with-current-buffer (find-file-noselect ts-file)
                      (unwind-protect
                          (let* ((cmd (alist-get 'prettier apheleia-formatters))
                                 (ctx (apheleia--formatter-context
                                       'prettier cmd nil)))
                            (unless ctx
                              (error "smoke: prettier context resolved to nil"))
                            (unless (equal (apheleia-formatter--arg1 ctx)
                                           (expand-file-name
                                            "node_modules/.bin/prettier"
                                            root))
                              (error "smoke: prettier arg1 wrong: %S"
                                     (apheleia-formatter--arg1 ctx)))
                            (unless (equal (apheleia-formatter--argv ctx)
                                           (list "--stdin-filepath" ts-file))
                              (error "smoke: prettier argv wrong: %S"
                                     (apheleia-formatter--argv ctx)))
                            (unless (equal (apheleia-formatter--name ctx)
                                           'prettier)
                              (error "smoke: prettier name wrong: %S"
                                     (apheleia-formatter--name ctx)))
                            (unless (eq (apheleia-formatter--stdin ctx)
                                        (current-buffer))
                              (error "smoke: prettier stdin buffer wrong"))
                            (princ "PASS: prettier -> local node_modules/.bin/prettier, argv splices filepath\n"))
                        (kill-buffer))))
                  ;; Go formatter: prefers goimports when found on PATH.
                  (let ((go-file (expand-file-name "cmd/main.go" root))
                        (fake-goimports (expand-file-name "bin/goimports" root)))
                    (tilde-apheleia-smoke--touch go-file)
                    (tilde-apheleia-smoke--mkexe fake-goimports)
                    (cl-letf (((symbol-function 'my/proj--vcs-root)
                               (lambda (&rest _) root)))
                      (with-current-buffer (find-file-noselect go-file)
                        (unwind-protect
                            (let ((exec-path (cons (file-name-directory
                                                    fake-goimports)
                                                   exec-path)))
                              (let* ((cmd (alist-get 'my/go-format
                                                     apheleia-formatters))
                                     (ctx (apheleia--formatter-context
                                           'my/go-format cmd nil)))
                                (unless ctx
                                  (error "smoke: go format context nil"))
                                (unless (equal (apheleia-formatter--arg1 ctx)
                                               fake-goimports)
                                  (error "smoke: goimports arg1 wrong: %S"
                                         (apheleia-formatter--arg1 ctx)))
                                (unless (null (apheleia-formatter--argv ctx))
                                  (error "smoke: goimports argv should be empty: %S"
                                         (apheleia-formatter--argv ctx)))
                                (princ "PASS: my/go-format -> goimports on exec-path, no extra args\n")))
                          (kill-buffer)))))))
            (delete-directory root t)))
      (advice-remove 'apheleia--execute-formatter-process
                     #'tilde-apheleia-smoke--refuse-process))
    (princ "PASS: apheleia dispatch integration smoke\n")))

(let* ((home (make-temp-file "tilde-apheleia-home-" t))
       (package-user-dir (expand-file-name package-user-dir))
       (user-emacs-directory (expand-file-name ".emacs.d/" home))
       (process-environment (copy-sequence process-environment)))
  (unwind-protect
      (progn
        (setenv "HOME" home)
        (setenv "XDG_CONFIG_HOME" (expand-file-name ".config" home))
        (setenv "XDG_CACHE_HOME" (expand-file-name ".cache" home))
        (when (and (fboundp 'startup-redirect-eln-cache)
                   (boundp 'native-comp-eln-load-path))
          (startup-redirect-eln-cache (expand-file-name "eln-cache/" home)))
        (catch 'tilde-apheleia-skip (tilde-apheleia-smoke--run)))
    (delete-directory home t)))

;;; emacs-apheleia-smoke.el ends here
