;;; emacs-workflow.el --- ERT tests for the workflow pilot -*- lexical-binding: t; -*-
;;
;; Run with:
;;   emacs -Q --batch -l scripts/tests/emacs-workflow.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; These tests never run a real test suite, never launch a subprocess,
;; and never mutate a real git repo. `compile' is stubbed to capture
;; the command it would have run; project detection is faked by
;; overriding `my/workflow--project-root' or by dropping marker files
;; into a temp tree.

(require 'ert)
(require 'cl-lib)
(require 'project)
;; Force python.el to define `python-info-current-defun' now, so tests can
;; override it with `cl-letf' before the workflow function calls
;; `(require 'python)' itself.
(require 'python)

(defconst tilde-workflow--repo-root
  (expand-file-name "../.."
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defconst tilde-workflow--modules-dir
  (expand-file-name "emacs/.emacs.d/modules" tilde-workflow--repo-root))

;; workflow.el does `(require 'proj-context)'; make it findable.
(add-to-list 'load-path tilde-workflow--modules-dir)

(defun tilde-workflow--load ()
  (load (expand-file-name "workflow.el" tilde-workflow--modules-dir)
        nil t t))

(defvar tilde-workflow--compile-calls nil
  "Alist of (default-directory . command) captured from stubbed compile.")

(defun tilde-workflow--install-stubs ()
  (setq tilde-workflow--compile-calls nil)
  (advice-add 'compile :override
              (lambda (cmd &rest _)
                (push (cons default-directory cmd)
                      tilde-workflow--compile-calls))
              '((name . tilde-workflow-compile)))
  ;; Prevent `package-installed-p' from touching a real archive on load.
  (unless (get 'package-installed-p 'tilde-workflow-stubbed)
    (advice-add 'package-installed-p :override (lambda (&rest _) nil)
                '((name . tilde-workflow-pkg)))
    (put 'package-installed-p 'tilde-workflow-stubbed t)))

(defun tilde-workflow--remove-stubs ()
  (advice-remove 'compile 'tilde-workflow-compile)
  (advice-remove 'package-installed-p 'tilde-workflow-pkg)
  (put 'package-installed-p 'tilde-workflow-stubbed nil))

(defmacro tilde-workflow--with-tree (root &rest body)
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,root (file-name-as-directory
                 (file-truename (make-temp-file "tilde-workflow-" t)))))
     (unwind-protect (progn ,@body)
       (when (file-directory-p ,root) (delete-directory ,root t)))))

(defmacro tilde-workflow--with-root (root &rest body)
  "Run BODY with `my/workflow--project-root' and the shared
`my/proj--vcs-root' both pinned to ROOT. Test temp directories
live outside any real project, so `project-current' returns nil
and `scope-root' would otherwise not resolve to ROOT for buffer-
less calls; pinning both keeps the pre-refactor semantics."
  (declare (indent 1) (debug (symbolp body)))
  `(cl-letf (((symbol-function 'my/workflow--project-root)
              (lambda () ,root))
             ((symbol-function 'my/proj--vcs-root)
              (lambda (&rest _) ,root)))
     ,@body))

(defun tilde-workflow--touch (path)
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert "")))

;; --------------------------------------------------------------------
;; Tests
;; --------------------------------------------------------------------

(ert-deftest tilde-workflow/keymaps-are-wired ()
  (tilde-workflow--install-stubs)
  (unwind-protect
      (progn
        (tilde-workflow--load)
        ;; C-c t map
        (should (eq (lookup-key my/workflow-test-map "p")
                    'my/workflow-run-project-tests))
        (should (eq (lookup-key my/workflow-test-map "f")
                    'my/workflow-run-file-tests))
        (should (eq (lookup-key my/workflow-test-map "n")
                    'my/workflow-run-nearest-test))
        (should (eq (lookup-key my/workflow-test-map "r")
                    'my/workflow-rerun-last-test))
        ;; C-c g map (autoloaded Magit symbols; do not require Magit)
        (should (eq (lookup-key my/workflow-magit-map "s") 'magit-status))
        (should (eq (lookup-key my/workflow-magit-map "l") 'magit-log-current))
        (should (eq (lookup-key my/workflow-magit-map "b") 'magit-blame-addition))
        ;; C-c s map
        (should (eq (lookup-key my/workflow-consult-search-map "l")
                    'consult-line))
        (should (eq (lookup-key my/workflow-consult-search-map "r")
                    'consult-ripgrep))
        ;; C-c p bound to the built-in project-prefix-map
        (should (eq (lookup-key global-map (kbd "C-c p"))
                    project-prefix-map))
        ;; Global test prefix hooked
        (should (eq (lookup-key global-map (kbd "C-c t"))
                    my/workflow-test-map)))
    (tilde-workflow--remove-stubs)))

(ert-deftest tilde-workflow/project-python-uses-uv-run-pytest ()
  (tilde-workflow--install-stubs)
  (unwind-protect
      (progn
        (tilde-workflow--load)
        (tilde-workflow--with-tree root
          (tilde-workflow--touch (expand-file-name "pyproject.toml" root))
          (tilde-workflow--with-root root
            (my/workflow-run-project-tests)
            (should (equal (cdar tilde-workflow--compile-calls)
                           "uv run pytest"))
            (should (equal (caar tilde-workflow--compile-calls) root)))))
    (tilde-workflow--remove-stubs)))

(ert-deftest tilde-workflow/project-go-uses-go-test-dot-dot-dot ()
  (tilde-workflow--install-stubs)
  (unwind-protect
      (progn
        (tilde-workflow--load)
        (tilde-workflow--with-tree root
          (tilde-workflow--touch (expand-file-name "go.mod" root))
          (tilde-workflow--with-root root
            (my/workflow-run-project-tests)
            (should (equal (cdar tilde-workflow--compile-calls)
                           "go test ./...")))))
    (tilde-workflow--remove-stubs)))

(ert-deftest tilde-workflow/project-js-respects-lockfile ()
  (tilde-workflow--install-stubs)
  (unwind-protect
      (progn
        (tilde-workflow--load)
        (dolist (case '(("pnpm-lock.yaml" . "pnpm test")
                        ("yarn.lock"      . "yarn test")
                        ("package-lock.json" . "npm test")))
          (tilde-workflow--with-tree root
            (tilde-workflow--touch (expand-file-name "package.json" root))
            (tilde-workflow--touch (expand-file-name (car case) root))
            (tilde-workflow--with-root root
              (my/workflow-run-project-tests)
              (should (equal (cdar tilde-workflow--compile-calls)
                             (cdr case)))))))
    (tilde-workflow--remove-stubs)))

(ert-deftest tilde-workflow/file-cmd-quotes-scary-names ()
  "Filenames with spaces and shell metacharacters must be quoted."
  (tilde-workflow--install-stubs)
  (unwind-protect
      (progn
        (tilde-workflow--load)
        (tilde-workflow--with-tree root
          (tilde-workflow--touch (expand-file-name "pyproject.toml" root))
          (let* ((file (expand-file-name "tests/weird name; rm -rf.py" root))
                 (cmd (my/workflow--file-cmd root :python file)))
            (should (string-prefix-p "uv run pytest " cmd))
            ;; Must contain the shell-quoted form, not the raw name.
            (should (string-match-p (regexp-quote (shell-quote-argument file))
                                    cmd))
            (should-not (string-match-p "rm -rf" (substring cmd 15))))))
    (tilde-workflow--remove-stubs)))

(ert-deftest tilde-workflow/go-file-cmd-uses-package-dir-not-file ()
  "Go file-scoped test must run the enclosing package (`./pkg'), not the file."
  (tilde-workflow--install-stubs)
  (unwind-protect
      (progn
        (tilde-workflow--load)
        (tilde-workflow--with-tree root
          (tilde-workflow--touch (expand-file-name "go.mod" root))
          (let* ((file (expand-file-name "pkg/a_test.go" root))
                 (cmd (my/workflow--file-cmd root :go file)))
            (should (string-match-p "go test " cmd))
            (should (string-match-p "pkg" cmd))
            (should-not (string-match-p "a_test\\.go" cmd)))))
    (tilde-workflow--remove-stubs)))

(ert-deftest tilde-workflow/rerun-is-per-project ()
  "Each project keeps its own last command; rerun in A does not use B's."
  (tilde-workflow--install-stubs)
  (unwind-protect
      (progn
        (tilde-workflow--load)
        (tilde-workflow--with-tree a
          (tilde-workflow--with-tree b
            (tilde-workflow--touch (expand-file-name "pyproject.toml" a))
            (tilde-workflow--touch (expand-file-name "go.mod" b))
            (tilde-workflow--with-root a (my/workflow-run-project-tests))
            (tilde-workflow--with-root b (my/workflow-run-project-tests))
            ;; Sanity: two distinct captures.
            (should (= 2 (length tilde-workflow--compile-calls)))
            ;; Rerun in A picks A's command, not B's.
            (setq tilde-workflow--compile-calls nil)
            (tilde-workflow--with-root a (my/workflow-rerun-last-test))
            (should (equal (cdar tilde-workflow--compile-calls)
                           "uv run pytest"))
            (should (equal (caar tilde-workflow--compile-calls) a))
            (tilde-workflow--with-root b (my/workflow-rerun-last-test))
            (should (equal (cdar tilde-workflow--compile-calls)
                           "go test ./...")))))
    (tilde-workflow--remove-stubs)))

(ert-deftest tilde-workflow/no-project-user-errors ()
  (tilde-workflow--install-stubs)
  (unwind-protect
      (progn
        (tilde-workflow--load)
        (cl-letf (((symbol-function 'project-current) (lambda (&optional _) nil)))
          (should-error (my/workflow-run-project-tests) :type 'user-error)
          (should-error (my/workflow-rerun-last-test)  :type 'user-error)))
    (tilde-workflow--remove-stubs)))

(ert-deftest tilde-workflow/js-nearest-user-errors ()
  "JS nearest must refuse to guess a runner; explicit configuration only."
  (tilde-workflow--install-stubs)
  (unwind-protect
      (progn
        (tilde-workflow--load)
        (tilde-workflow--with-tree root
          (tilde-workflow--touch (expand-file-name "package.json" root))
          (tilde-workflow--touch (expand-file-name "pnpm-lock.yaml" root))
          (should-error
           (my/workflow--nearest-cmd root :js
                                     (expand-file-name "a.test.js" root))
           :type 'user-error)))
    (tilde-workflow--remove-stubs)))

(ert-deftest tilde-workflow/unsupported-project-user-errors ()
  (tilde-workflow--install-stubs)
  (unwind-protect
      (progn
        (tilde-workflow--load)
        (tilde-workflow--with-tree root
          (tilde-workflow--with-root root
            (should-error (my/workflow-run-project-tests) :type 'user-error))))
    (tilde-workflow--remove-stubs)))

(ert-deftest tilde-workflow/js-project-without-lockfile-user-errors ()
  (tilde-workflow--install-stubs)
  (unwind-protect
      (progn
        (tilde-workflow--load)
        (tilde-workflow--with-tree root
          (tilde-workflow--touch (expand-file-name "package.json" root))
          (tilde-workflow--with-root root
            (should-error (my/workflow-run-project-tests) :type 'user-error))))
    (tilde-workflow--remove-stubs)))

(ert-deftest tilde-workflow/project-override-is-not-auto-safe ()
  "The buffer-local override must not be marked auto-safe for arbitrary strings."
  (tilde-workflow--install-stubs)
  (unwind-protect
      (progn
        (tilde-workflow--load)
        ;; No `safe-local-variable' predicate installed -> Emacs prompts.
        (should-not (get 'my/workflow-project-test-command 'safe-local-variable))
        ;; When set buffer-locally, it takes precedence over auto-detection.
        (tilde-workflow--with-tree root
          (tilde-workflow--touch (expand-file-name "pyproject.toml" root))
          (with-temp-buffer
            (setq-local my/workflow-project-test-command "make test-fast")
            (tilde-workflow--with-root root
              (my/workflow-run-project-tests)
              (should (equal (cdar tilde-workflow--compile-calls)
                             "make test-fast"))))))
    (tilde-workflow--remove-stubs)))

(ert-deftest tilde-workflow/python-nearest-uses-defun-and-quotes ()
  (tilde-workflow--install-stubs)
  (unwind-protect
      (progn
        (tilde-workflow--load)
        (tilde-workflow--with-tree root
          (let ((file (expand-file-name "tests/test_x.py" root)))
            (cl-letf (((symbol-function 'python-info-current-defun)
                       (lambda () "TestX.test_thing")))
              (let ((cmd (my/workflow--python-nearest file)))
                (should (equal cmd
                               (concat "uv run pytest "
                                       (shell-quote-argument
                                        (concat file "::TestX::test_thing")))))))
            (cl-letf (((symbol-function 'python-info-current-defun)
                       (lambda () nil)))
              (should-error (my/workflow--python-nearest file) :type 'user-error)))))
    (tilde-workflow--remove-stubs)))

(ert-deftest tilde-workflow/go-nearest-anchors-on-test-func ()
  (tilde-workflow--install-stubs)
  (unwind-protect
      (progn
        (tilde-workflow--load)
        (tilde-workflow--with-tree root
          (let ((file (expand-file-name "pkg/a_test.go" root)))
            (make-directory (file-name-directory file) t)
            (with-temp-buffer
              (setq buffer-file-name file)
              (insert "package pkg\n\nfunc TestFoo(t *testing.T) {\n\t// here\n")
              (goto-char (point-max))
              (let ((cmd (my/workflow--go-nearest root file)))
                (should (string-match-p "go test -run " cmd))
                (should (string-match-p
                         (regexp-quote (shell-quote-argument "^TestFoo$"))
                         cmd))
                (should (string-match-p "pkg" cmd))))
            (with-temp-buffer
              (setq buffer-file-name file)
              (insert "package pkg\n\nfunc TestOld(t *testing.T) {}\nfunc helper() {}\n")
              (goto-char (point-max))
              (should-error (my/workflow--go-nearest root file)
                            :type 'user-error)))))
    (tilde-workflow--remove-stubs)))

(provide 'emacs-workflow)
;;; emacs-workflow.el ends here
