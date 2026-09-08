;;; emacs-config.el --- ERT tests for tilde Emacs modules -*- lexical-binding: t; -*-
;;
;; Run with:
;;   emacs -Q --batch -l scripts/tests/emacs-config.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; These tests deliberately DO NOT load the user's full init file. They
;; load individual modules under stubbed dependencies so we can exercise
;; correctness of the extraction without touching MELPA, without ever
;; starting a real language server, and without downloading a
;; tree-sitter grammar. Stubs are grouped in `tilde-test--install-stubs'
;; below so they are easy to audit.
;;
;; What these tests cover:
;;
;;   * The lsp module installs exactly one CAPF completion hook and does
;;     not overwrite `lsp-completion-provider' with :none.
;;   * Language modules (typescript, python, go) do NOT add their own
;;     lsp-mode-hook callback and rely on the shared setup.
;;   * TypeScript project-server discovery finds a node_modules/.bin
;;     executable and falls back to `executable-find'.
;;   * The Python venv helpers activate A, then B, then deactivate, and
;;     restore the pre-A snapshot exactly. No path accumulation.
;;   * Without a project and without a $HOME/.venv, `uv-activate' emits
;;     a plain message and leaves buffer state untouched.
;;   * The tree-sitter module registers sources but never calls the
;;     installer at load time; remaps depend on grammar availability.
;;
;; What these tests do NOT cover:
;;
;;   * They do not launch a real gopls, pyright, or
;;     typescript-language-server. `lsp-deferred' and the actual LSP
;;     handshake are stubbed to a no-op that just records the call.
;;   * They do not verify Corfu or Cape frontends behave correctly in
;;     an interactive frame - batch Emacs has no frame.
;;   * They do not exercise exec-path-from-shell, which is guarded to
;;     `(display-graphic-p)' and `(not noninteractive)' in the module.

(require 'ert)
(require 'seq)
(require 'cl-lib)
;; The real python.el and treesit are required so their global variables
;; (`python-shell-interpreter', `treesit-language-source-alist') exist as
;; genuine defvar-backed specials. Without this, `let'-bindings on those
;; names would silently become lexical under this file's lexical scope
;; and the module code (which uses `setq-local' and `add-to-list' on the
;; dynamic name) would not observe the test's setup.
(require 'python)
(when (fboundp 'treesit-available-p) (require 'treesit))

(defconst tilde-test--repo-root
  (expand-file-name "../.."
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defconst tilde-test--emacs-dir
  (expand-file-name "emacs/.emacs.d" tilde-test--repo-root))

(defconst tilde-test--modules-dir
  (expand-file-name "modules" tilde-test--emacs-dir))

(defun tilde-test--load-module (name)
  "Load module NAME from the tracked emacs.d without going through init.el."
  (load (expand-file-name (concat name ".el") tilde-test--modules-dir)
        nil t t))

;; --------------------------------------------------------------------
;; Test stubs. Kept separate from the real Emacs API so it's obvious
;; what is fake. Nothing here talks to a real server or the network.
;; --------------------------------------------------------------------

(defvar tilde-test--lsp-deferred-calls 0)
(defvar tilde-test--lsp-mode nil)

(defun tilde-test--install-stubs ()
  "Install harmless replacements for lsp-mode and friends."
  ;; lsp-mode surface used by the config.
  (unless (fboundp 'lsp-deferred)
    (defalias 'lsp-deferred
      (lambda (&rest _)
        (cl-incf tilde-test--lsp-deferred-calls)
        nil)))
  (unless (fboundp 'lsp-completion-at-point)
    (defalias 'lsp-completion-at-point (lambda () nil)))
  (unless (boundp 'lsp-mode-map)
    (defvar lsp-mode-map (make-sparse-keymap)))
  (unless (boundp 'lsp-mode-hook)
    (defvar lsp-mode-hook nil))
  (unless (boundp 'lsp-completion-provider)
    (defvar lsp-completion-provider :capf))
  (unless (boundp 'lsp-clients-typescript-tls-path)
    (defvar lsp-clients-typescript-tls-path "typescript-language-server"))
  (unless (boundp 'lsp-clients-typescript-prefer-use-project-ts-server)
    (defvar lsp-clients-typescript-prefer-use-project-ts-server nil))
  ;; Stub `package-installed-p' to a controllable set.
  (defvar tilde-test--installed-packages '())
  (advice-add 'package-installed-p :override
              (lambda (pkg &optional _)
                (memq pkg tilde-test--installed-packages))
              '((name . tilde-test-installed)))
  ;; Stub `yas-minor-mode' so language-mode setup functions can run
  ;; without loading yasnippet.
  (unless (fboundp 'yas-minor-mode)
    (defalias 'yas-minor-mode (lambda (&rest _) nil))))

(defun tilde-test--remove-stubs ()
  (ignore-errors (advice-remove 'package-installed-p 'tilde-test-installed)))

;; --------------------------------------------------------------------
;; Utility helpers
;; --------------------------------------------------------------------

(defmacro tilde-test--with-temp-tree (root &rest body)
  "Bind ROOT to a fresh temp directory, evaluate BODY, delete after."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,root (make-temp-file "tilde-emacs-test-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,root)
         (delete-directory ,root t)))))

;; --------------------------------------------------------------------
;; Tests
;; --------------------------------------------------------------------

(ert-deftest tilde/lsp-installs-single-capf-hook ()
  "The lsp module installs a shared completion hook and no :none override."
  (tilde-test--install-stubs)
  (unwind-protect
      (let ((lsp-mode-hook nil)
            (lsp-completion-provider :capf))
        (tilde-test--load-module "lsp")
        (should (memq #'my/lsp-completion-setup lsp-mode-hook))
        (should (eq lsp-completion-provider :capf))
        ;; Confirm the source file itself no longer overrides to :none.
        (let ((body (with-temp-buffer
                      (insert-file-contents
                       (expand-file-name "lsp.el" tilde-test--modules-dir))
                      (buffer-string))))
          (should-not (string-match-p ":none" body))))
    (tilde-test--remove-stubs)))

(ert-deftest tilde/language-modules-have-no-completion-callback ()
  "typescript, python, go must rely on the shared completion hook."
  (dolist (module '("typescript" "python" "go"))
    (let ((body (with-temp-buffer
                  (insert-file-contents
                   (expand-file-name (concat module ".el")
                                     tilde-test--modules-dir))
                  (buffer-string))))
      ;; No direct hook registration on lsp-mode-hook from this module.
      (should-not (string-match-p "(add-hook[ \t\n]+'lsp-mode-hook" body))
      ;; No re-assignment of completion-at-point-functions from this module.
      (should-not
       (string-match-p "completion-at-point-functions" body)))))

(ert-deftest tilde/typescript-project-server-discovery ()
  "Prefer node_modules/.bin/typescript-language-server, else executable-find."
  (tilde-test--install-stubs)
  (unwind-protect
      (progn
        (tilde-test--load-module "typescript")
        (tilde-test--with-temp-tree root
          (let* ((bin-dir (expand-file-name "node_modules/.bin" root))
                 (exe (expand-file-name "typescript-language-server" bin-dir))
                 (file (expand-file-name "src/a.ts" root)))
            (make-directory bin-dir t)
            (make-directory (expand-file-name "src" root) t)
            (with-temp-file exe (insert "#!/bin/sh\nexit 0\n"))
            (set-file-modes exe #o755)
            (with-temp-file file (insert "export const x = 1;\n"))
            (with-current-buffer (find-file-noselect file)
              (unwind-protect
                  (progn
                    (should (equal (my/typescript-project-server) exe))
                    (my/typescript-configure-server)
                    (should (equal lsp-clients-typescript-tls-path exe))
                    (should lsp-clients-typescript-prefer-use-project-ts-server))
                (kill-buffer)))))
        ;; Fallback path: no project-local exe, but executable-find hits.
        (tilde-test--with-temp-tree root
          (let ((fake-exe (expand-file-name "typescript-language-server" root)))
            (with-temp-file fake-exe (insert "#!/bin/sh\nexit 0\n"))
            (set-file-modes fake-exe #o755)
            (let ((exec-path (cons root exec-path))
                  (buf (find-file-noselect
                        (expand-file-name "isolated.ts" root))))
              (unwind-protect
                  (with-current-buffer buf
                    (should (null (my/typescript-project-server)))
                    (my/typescript-configure-server)
                    (should (equal lsp-clients-typescript-tls-path fake-exe)))
                (kill-buffer buf))))))
    (tilde-test--remove-stubs)))

(ert-deftest tilde/venv-A-then-B-then-deactivate-restores-snapshot ()
  "Activate A, activate B, deactivate: state matches pre-A exactly."
  (tilde-test--install-stubs)
  (unwind-protect
      (progn
        (tilde-test--load-module "environments")
        (tilde-test--with-temp-tree root
          (let* ((va (expand-file-name "A" root))
                 (vb (expand-file-name "B" root))
                 (pa (expand-file-name "bin/python" va))
                 (pb (expand-file-name "bin/python" vb)))
            (make-directory (expand-file-name "bin" va) t)
            (make-directory (expand-file-name "bin" vb) t)
            (with-temp-file pa (insert "#!/bin/sh\n")) (set-file-modes pa #o755)
            (with-temp-file pb (insert "#!/bin/sh\n")) (set-file-modes pb #o755)
            (with-temp-buffer
              ;; Baseline snapshot: keep local copies to compare against.
              (let ((python-shell-interpreter "python")
                    (process-environment (list "PATH=/original/bin"
                                               "OTHER=keep"))
                    (exec-path '("/original/bin")))
                (make-local-variable 'python-shell-interpreter)
                (make-local-variable 'process-environment)
                (make-local-variable 'exec-path)
                (let ((baseline-pe (copy-sequence process-environment))
                      (baseline-ep (copy-sequence exec-path))
                      (baseline-py python-shell-interpreter))
                  (my/activate-venv va "bin/python")
                  (should (equal python-shell-interpreter pa))
                  (should (member (expand-file-name "bin/" va) exec-path))
                  (my/activate-venv vb "bin/python")
                  (should (equal python-shell-interpreter pb))
                  ;; After switching to B, A's bin must be gone from exec-path.
                  (should-not (member (expand-file-name "bin/" va) exec-path))
                  (should (member (expand-file-name "bin/" vb) exec-path))
                  ;; Deactivate: back to baseline exactly.
                  (my/deactivate-current-venv)
                  (should (equal process-environment baseline-pe))
                  (should (equal exec-path baseline-ep))
                  (should (equal python-shell-interpreter baseline-py))
                  ;; And no accumulated venv bin dirs anywhere.
                  (should-not (member (expand-file-name "bin/" va) exec-path))
                  (should-not (member (expand-file-name "bin/" vb) exec-path))))))))
    (tilde-test--remove-stubs)))

(ert-deftest tilde/uv-activate-no-project-no-home-venv-is-silent ()
  "No project and no $HOME/.venv: uv-activate emits a message, no error."
  (tilde-test--install-stubs)
  (unwind-protect
      (progn
        (tilde-test--load-module "environments")
        (tilde-test--with-temp-tree root
          (let ((process-environment
                 (cons (format "HOME=%s" root) process-environment))
                (default-directory root)
                (msg nil))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq msg (apply #'format fmt args)))))
              (with-temp-buffer
                (let ((baseline-py python-shell-interpreter)
                      (baseline-ep (copy-sequence exec-path)))
                  (uv-activate)
                  (should (stringp msg))
                  (should (string-match-p "No .venv" msg))
                  (should (equal python-shell-interpreter baseline-py))
                  (should (equal exec-path baseline-ep))))))))
    (tilde-test--remove-stubs)))

(ert-deftest tilde/treesitter-registers-sources-does-not-install ()
  "Loading the treesitter module registers grammar sources but never installs."
  (tilde-test--install-stubs)
  (unwind-protect
      (let ((install-called nil)
            (treesit-language-source-alist nil))
        (cl-letf (((symbol-function 'treesit-install-language-grammar)
                   (lambda (&rest _) (setq install-called t)))
                  ((symbol-function 'treesit-available-p) (lambda () t))
                  ((symbol-function 'treesit-language-available-p)
                   (lambda (_) nil)))
          (tilde-test--load-module "treesitter")
          (should-not install-called)
          (dolist (lang '(typescript tsx javascript json python))
            (should (assq lang treesit-language-source-alist)))
          ;; No grammars "available" -> no remaps, no auto-mode entries.
          (dolist (mode '(typescript-mode js-mode json-mode))
            (should-not (assq mode major-mode-remap-alist)))))
    (tilde-test--remove-stubs)))

(ert-deftest tilde/init-load-order-environment-before-language ()
  "environment loads before typescript/python/go/elisp; environments before them too."
  (let ((init (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "init.el" tilde-test--emacs-dir))
                (buffer-string))))
    (cl-flet ((pos (mod) (string-match (format "\"%s\"" mod) init)))
      (should (< (pos "environment") (pos "typescript")))
      (should (< (pos "environment") (pos "python")))
      (should (< (pos "environment") (pos "go")))
      (should (< (pos "environments") (pos "typescript")))
      (should (< (pos "environments") (pos "python")))
      (should (< (pos "environments") (pos "go"))))))

(ert-deftest tilde/packages-does-not-refresh-or-install-at-load ()
  "Loading the packages module must not refresh archives or install anything."
  (let ((refresh-called nil)
        (install-called nil))
    (cl-letf (((symbol-function 'package-refresh-contents)
               (lambda (&rest _) (setq refresh-called t)))
              ((symbol-function 'package-install)
               (lambda (&rest _) (setq install-called t))))
      (tilde-test--load-module "packages")
      (should-not refresh-called)
      (should-not install-called)
      (should (fboundp 'my/install-packages))
      (should (memq 'exec-path-from-shell my-packages)))))

(ert-deftest tilde/typescript-server-is-buffer-local-on-first-call ()
  (tilde-test--install-stubs)
  (unwind-protect
      (progn
        (tilde-test--load-module "typescript")
        (let ((global (default-value 'lsp-clients-typescript-tls-path)))
          (with-temp-buffer
            (cl-letf (((symbol-function 'my/typescript-project-server)
                       (lambda () "/project/node_modules/.bin/server")))
              (my/typescript-configure-server)
              (should (local-variable-p 'lsp-clients-typescript-tls-path))
              (should (equal lsp-clients-typescript-tls-path
                             "/project/node_modules/.bin/server"))))
          (should (equal global (default-value 'lsp-clients-typescript-tls-path)))))
    (tilde-test--remove-stubs)))

(ert-deftest tilde/completion-is-ready-before-first-language-start ()
  (tilde-test--install-stubs)
  (unwind-protect
      (progn
        (tilde-test--load-module "lsp")
        (tilde-test--load-module "typescript")
        (with-temp-buffer
          (cl-letf (((symbol-function 'lsp-deferred)
                     (lambda ()
                       (should (memq #'my/lsp-completion-setup lsp-mode-hook))
                       (setq-local lsp-mode t)
                       (run-hooks 'lsp-mode-hook)
                       (should (eq (car completion-at-point-functions)
                                   'lsp-completion-at-point)))))
            (my/setup-typescript-development))))
    (tilde-test--remove-stubs)))

(ert-deftest tilde/venv-does-not-change-global-environment ()
  (tilde-test--load-module "environments")
  (tilde-test--with-temp-tree root
    (let ((exe (expand-file-name "base/bin/python" root))
          (before (copy-sequence (default-value 'process-environment))))
      (make-directory (file-name-directory exe) t)
      (with-temp-file exe (insert "#!/bin/sh\n"))
      (set-file-modes exe #o755)
      (with-temp-buffer
        (my/activate-venv root "base/bin/python")
        (should (equal (getenv "VIRTUAL_ENV") (expand-file-name "base" root)))
        (should (equal before (default-value 'process-environment)))))))

(ert-deftest tilde/invalid-venv-preserves-active-state ()
  (tilde-test--load-module "environments")
  (with-temp-buffer
    (let ((before (copy-sequence process-environment)))
      (should-error (my/activate-venv "/absent-tilde-test" "bin/python") :type 'user-error)
      (should (equal before process-environment))
      (should-not my/venv--snapshot))))

(provide 'emacs-config)
;;; emacs-config.el ends here
