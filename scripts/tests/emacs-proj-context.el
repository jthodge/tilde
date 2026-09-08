;;; emacs-proj-context.el --- ERT tests for shared project context + formatters -*- lexical-binding: t; -*-
;;
;; Covers:
;;   * `proj-context.el' language / root / package-manager / node_modules
;;     resolution in mixed and nested repositories, including paths with
;;     spaces and hoisted binaries.
;;   * `workflow.el' end-to-end selection: a TypeScript file in a mixed
;;     Python + JS + Go repo runs the JS runner, not pytest, and the
;;     three scopes (project / file / nearest) plus rerun all share one
;;     resolved scope root per (buffer-file, project).
;;   * Fail-closed kind detection: a `.txt' buffer under a Python
;;     project root does not silently become `:python'.
;;   * Per-scope buffer-local overrides (`my/workflow-project-test-command',
;;     `-file-test-command', `-nearest-test-command') and their lack of
;;     `safe-local-variable' predicates.
;;   * `environments.el' venv root resolution honours the bounded
;;     `my/proj-context' when a Python file resolves a nested root.
;;   * `typescript.el' TS-server discovery uses the shared
;;     `my/proj-find-node-bin' so a hoisted root binary wins when the
;;     nested workspace omits it.
;;   * Apheleia is the sole Go format owner: no `lsp-format-buffer' or
;;     `lsp-organize-imports' wired on `before-save-hook', and the module
;;     no longer defines `lsp-go-install-save-hooks'.
;;   * Prettier resolver picks the project-local binary for JS/TS and
;;     for formatter assets (JSON), and falls back to a hoisted root
;;     binary when the nearest workspace omits `.bin/prettier'.
;;
;; No test runs a real formatter, launches an LSP server, or touches
;; the network. `compile' is advised to capture its inputs. The real
;; Apheleia integration test (which loads the installed package and
;; calls `apheleia--formatter-context') lives in
;; `scripts/tests/emacs-apheleia-smoke.el' and runs under `make smoke'
;; only when the package is present.
;;
;; Run:
;;   emacs -Q --batch -l scripts/tests/emacs-proj-context.el \\
;;         -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'project)
(require 'python)

(defconst tilde-pctx--repo-root
  (expand-file-name "../.."
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defconst tilde-pctx--modules-dir
  (expand-file-name "emacs/.emacs.d/modules" tilde-pctx--repo-root))

(add-to-list 'load-path tilde-pctx--modules-dir)

(defun tilde-pctx--load-module (name)
  (load (expand-file-name (concat name ".el") tilde-pctx--modules-dir)
        nil t t))

(defvar tilde-pctx--compile-calls nil
  "List of (default-directory . command) captured from stubbed compile.")

(defun tilde-pctx--install-stubs ()
  (setq tilde-pctx--compile-calls nil)
  (advice-add 'compile :override
              (lambda (cmd &rest _)
                (push (cons default-directory cmd)
                      tilde-pctx--compile-calls))
              '((name . tilde-pctx-compile)))
  (unless (get 'package-installed-p 'tilde-pctx-stubbed)
    (advice-add 'package-installed-p :override (lambda (&rest _) nil)
                '((name . tilde-pctx-pkg)))
    (put 'package-installed-p 'tilde-pctx-stubbed t)))

(defun tilde-pctx--remove-stubs ()
  (advice-remove 'compile 'tilde-pctx-compile)
  (advice-remove 'package-installed-p 'tilde-pctx-pkg)
  (put 'package-installed-p 'tilde-pctx-stubbed nil))

(defmacro tilde-pctx--with-tree (root &rest body)
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,root (file-name-as-directory
                 (file-truename (make-temp-file "tilde-pctx-" t)))))
     (unwind-protect (progn ,@body)
       (when (file-directory-p ,root) (delete-directory ,root t)))))

(defmacro tilde-pctx--with-root (root &rest body)
  "Pin `my/workflow--project-root' and `my/proj--vcs-root' to ROOT.
The vcs-root override matters because our temp directories live
outside any real project, so `project-current' returns nil from
them and boundary detection would default to \"/\". Values here
are lexical (no `file-truename'), matching what the resolver
returns."
  (declare (indent 1) (debug (symbolp body)))
  `(cl-letf (((symbol-function 'my/workflow--project-root)
              (lambda () ,root))
             ((symbol-function 'my/proj--vcs-root)
              (lambda (&rest _) ,root)))
     ,@body))

(defun tilde-pctx--touch (path)
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert "")))

(defun tilde-pctx--mkexe (path)
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert "#!/bin/sh\n"))
  (set-file-modes path #o755))

;; --------------------------------------------------------------------
;; Resolver tests
;; --------------------------------------------------------------------

(ert-deftest tilde-pctx/language-from-extension-not-marker ()
  "A .ts file in a repo containing pyproject.toml + package.json +
pnpm-lock.yaml + go.mod must be detected as :ts, never :python."
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--with-tree root
          (dolist (m '("pyproject.toml" "package.json" "pnpm-lock.yaml"
                       "go.mod"))
            (tilde-pctx--touch (expand-file-name m root)))
          (let ((ts (expand-file-name "src/a.ts" root)))
            (tilde-pctx--touch ts)
            (tilde-pctx--with-root root
              (let ((ctx (my/proj-context ts)))
                (should (eq (plist-get ctx :language) :ts))
                (should (equal (plist-get ctx :root) root))
                (should (eq (plist-get ctx :package-manager) :pnpm)))))))
    (tilde-pctx--remove-stubs)))

(ert-deftest tilde-pctx/language-python-when-marker-and-py-file-align ()
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--with-tree root
          (tilde-pctx--touch (expand-file-name "pyproject.toml" root))
          (tilde-pctx--touch (expand-file-name "package.json" root))
          (let ((py (expand-file-name "src/a.py" root)))
            (tilde-pctx--touch py)
            (tilde-pctx--with-root root
              (let ((ctx (my/proj-context py)))
                (should (eq (plist-get ctx :language) :python))
                (should (equal (plist-get ctx :root) root)))))))
    (tilde-pctx--remove-stubs)))

(ert-deftest tilde-pctx/nested-workspace-lockfile-at-monorepo-root ()
  "Nearest package.json is the inner workspace; nearest lockfile is at
the monorepo root. :root = inner, :package-manager = :pnpm."
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--with-tree root
          (tilde-pctx--touch (expand-file-name "pnpm-lock.yaml" root))
          (let* ((inner (file-name-as-directory
                         (expand-file-name "packages/app" root)))
                 (file (expand-file-name "src/x.ts" inner)))
            (tilde-pctx--touch (expand-file-name "package.json" inner))
            (tilde-pctx--touch file)
            (tilde-pctx--with-root root
              (let ((ctx (my/proj-context file)))
                (should (equal (plist-get ctx :root) inner))
                (should (eq (plist-get ctx :package-manager) :pnpm)))))))
    (tilde-pctx--remove-stubs)))

(ert-deftest tilde-pctx/upward-search-stops-at-vcs-root ()
  "A lockfile living above the pinned vcs-root must be invisible."
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--with-tree parent
          ;; Lockfile at PARENT is out of bounds because vcs-root is INNER.
          (tilde-pctx--touch (expand-file-name "pnpm-lock.yaml" parent))
          (let* ((inner (file-name-as-directory
                         (expand-file-name "repo" parent)))
                 (file (expand-file-name "src/x.ts" inner)))
            (tilde-pctx--touch (expand-file-name "package.json" inner))
            (tilde-pctx--touch file)
            (tilde-pctx--with-root inner
              (let ((ctx (my/proj-context file)))
                (should (equal (plist-get ctx :root) inner))
                (should (null (plist-get ctx :package-manager))))))))
    (tilde-pctx--remove-stubs)))

(ert-deftest tilde-pctx/package-manager-choice-npm-yarn-pnpm ()
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (dolist (case '(("pnpm-lock.yaml"     . :pnpm)
                        ("yarn.lock"          . :yarn)
                        ("package-lock.json"  . :npm)
                        ("npm-shrinkwrap.json" . :npm)))
          (tilde-pctx--with-tree root
            (tilde-pctx--touch (expand-file-name "package.json" root))
            (tilde-pctx--touch (expand-file-name (car case) root))
            (let ((file (expand-file-name "index.js" root)))
              (tilde-pctx--touch file)
              (tilde-pctx--with-root root
                (should (eq (plist-get (my/proj-context file)
                                       :package-manager)
                            (cdr case))))))))
    (tilde-pctx--remove-stubs)))

(ert-deftest tilde-pctx/paths-with-spaces-resolve ()
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--with-tree root
          (let* ((wsroot (file-name-as-directory
                          (expand-file-name "a b c" root)))
                 (file (expand-file-name "src/mod name.ts" wsroot)))
            (tilde-pctx--touch (expand-file-name "package.json" wsroot))
            (tilde-pctx--touch (expand-file-name "yarn.lock" wsroot))
            (tilde-pctx--touch file)
            (tilde-pctx--with-root root
              (let ((ctx (my/proj-context file)))
                (should (equal (plist-get ctx :root) wsroot))
                (should (eq (plist-get ctx :package-manager) :yarn)))))))
    (tilde-pctx--remove-stubs)))

(ert-deftest tilde-pctx/unknown-language-is-nil-not-python ()
  "A file with no recognised extension yields :language nil even
when pyproject.toml exists next to it."
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--with-tree root
          (tilde-pctx--touch (expand-file-name "pyproject.toml" root))
          (let ((file (expand-file-name "notes.txt" root)))
            (tilde-pctx--touch file)
            (tilde-pctx--with-root root
              (let ((ctx (my/proj-context file)))
                (should (null (plist-get ctx :language)))
                (should (null (plist-get ctx :root))))))))
    (tilde-pctx--remove-stubs)))

;; --------------------------------------------------------------------
;; Workflow end-to-end selection over mixed / nested repos
;; --------------------------------------------------------------------

(ert-deftest tilde-pctx/workflow-mixed-repo-picks-language-from-file ()
  "TS file in mixed Python+JS+Go repo runs `pnpm test', not pytest."
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--load-module "workflow")
        (tilde-pctx--with-tree root
          (dolist (m '("pyproject.toml" "package.json" "pnpm-lock.yaml"
                       "go.mod"))
            (tilde-pctx--touch (expand-file-name m root)))
          (let ((ts (expand-file-name "src/a.ts" root)))
            (tilde-pctx--touch ts)
            (with-current-buffer (generate-new-buffer " *pctx-ts*")
              (unwind-protect
                  (progn
                    (setq buffer-file-name ts)
                    (tilde-pctx--with-root root
                      (my/workflow-run-project-tests)
                      (should (equal (cdar tilde-pctx--compile-calls)
                                     "pnpm test"))
                      (should (equal (caar tilde-pctx--compile-calls)
                                     root))))
                (kill-buffer))))))
    (tilde-pctx--remove-stubs)))

(ert-deftest tilde-pctx/workflow-nested-workspace-file-cwd-is-inner ()
  "File tests in a nested workspace run with cwd = inner package.json dir."
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--load-module "workflow")
        (tilde-pctx--with-tree root
          (tilde-pctx--touch (expand-file-name "pnpm-lock.yaml" root))
          (let* ((inner (file-name-as-directory
                         (expand-file-name "packages/app" root)))
                 (file (expand-file-name "src/x.test.ts" inner)))
            (tilde-pctx--touch (expand-file-name "package.json" inner))
            (tilde-pctx--touch file)
            (with-current-buffer (generate-new-buffer " *pctx-nested*")
              (unwind-protect
                  (progn
                    (setq buffer-file-name file)
                    (tilde-pctx--with-root root
                      (my/workflow-run-file-tests)
                      (should (string-prefix-p "pnpm test -- "
                                               (cdar tilde-pctx--compile-calls)))
                      (should (equal (caar tilde-pctx--compile-calls)
                                     inner))))
                (kill-buffer))))))
    (tilde-pctx--remove-stubs)))

(ert-deftest tilde-pctx/workflow-mixed-project-no-file-user-errors ()
  "With no buffer file, a repo with multiple language markers must
`user-error' rather than guess Python from pyproject.toml."
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--load-module "workflow")
        (tilde-pctx--with-tree root
          (dolist (m '("pyproject.toml" "package.json" "go.mod"))
            (tilde-pctx--touch (expand-file-name m root)))
          (tilde-pctx--with-root root
            (should-error (my/workflow-run-project-tests)
                          :type 'user-error))))
    (tilde-pctx--remove-stubs)))

;; --------------------------------------------------------------------
;; Per-scope overrides
;; --------------------------------------------------------------------

(ert-deftest tilde-pctx/overrides-are-per-scope-and-not-auto-safe ()
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--load-module "workflow")
        ;; None of the three overrides is marked auto-safe.
        (dolist (v '(my/workflow-project-test-command
                     my/workflow-file-test-command
                     my/workflow-nearest-test-command))
          (should-not (get v 'safe-local-variable)))
        (tilde-pctx--with-tree root
          (tilde-pctx--touch (expand-file-name "pyproject.toml" root))
          (let ((file (expand-file-name "tests/test_x.py" root)))
            (tilde-pctx--touch file)
            ;; project scope override
            (with-current-buffer (generate-new-buffer " *pctx-p*")
              (unwind-protect
                  (progn
                    (setq buffer-file-name file)
                    (setq-local my/workflow-project-test-command
                                "make test-fast")
                    (tilde-pctx--with-root root
                      (my/workflow-run-project-tests)
                      (should (equal (cdar tilde-pctx--compile-calls)
                                     "make test-fast"))))
                (kill-buffer)))
            (setq tilde-pctx--compile-calls nil)
            ;; file scope override -- must NOT be used for project scope
            (with-current-buffer (generate-new-buffer " *pctx-f*")
              (unwind-protect
                  (progn
                    (setq buffer-file-name file)
                    (setq-local my/workflow-file-test-command
                                "pytest -x tests/test_x.py")
                    (tilde-pctx--with-root root
                      ;; project uses auto-detection, ignores the file override
                      (my/workflow-run-project-tests)
                      (should (equal (cdar tilde-pctx--compile-calls)
                                     "uv run pytest"))
                      ;; file scope picks up its own override
                      (my/workflow-run-file-tests)
                      (should (equal (cdar tilde-pctx--compile-calls)
                                     "pytest -x tests/test_x.py"))))
                (kill-buffer)))
            (setq tilde-pctx--compile-calls nil)
            ;; nearest scope override
            (with-current-buffer (generate-new-buffer " *pctx-n*")
              (unwind-protect
                  (progn
                    (setq buffer-file-name file)
                    (setq-local my/workflow-nearest-test-command
                                "pytest -k my_case")
                    (tilde-pctx--with-root root
                      (my/workflow-run-nearest-test)
                      (should (equal (cdar tilde-pctx--compile-calls)
                                     "pytest -k my_case"))))
                (kill-buffer))))))
    (tilde-pctx--remove-stubs)))

;; --------------------------------------------------------------------
;; Apheleia / Go format ownership
;; --------------------------------------------------------------------

(ert-deftest tilde-pctx/lsp-module-does-not-wire-go-save-hooks ()
  "The lsp module must not define `lsp-go-install-save-hooks' or wire
`lsp-format-buffer' / `lsp-organize-imports' onto any hook. Both
symbols may still appear in a removal comment; what matters is
that no live `add-hook' or `defun' invokes them."
  (let ((body (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "lsp.el" tilde-pctx--modules-dir))
                (buffer-string))))
    (should-not (string-match-p "defun[ \t\n]+lsp-go-install-save-hooks" body))
    (should-not (string-match-p "add-hook[ \t\n]+'go-mode-hook" body))
    (should-not
     (string-match-p
      "add-hook[ \t\n]+'before-save-hook[ \t\n]+#'lsp-format-buffer" body))
    (should-not
     (string-match-p
      "add-hook[ \t\n]+'before-save-hook[ \t\n]+#'lsp-organize-imports" body))
    ;; Sanity: the removal-comment explains ownership.
    (should (string-match-p "Apheleia" body))))

(ert-deftest tilde-pctx/go-mode-hook-is-clean-of-format-owners ()
  "After loading lsp.el under stubs, `go-mode-hook' must not carry a
function that wires `before-save-hook' formatters."
  (tilde-pctx--install-stubs)
  (unwind-protect
      (let ((go-mode-hook nil))
        ;; Provide the minimal lsp-mode surface so lsp.el loads.
        (unless (boundp 'lsp-mode-hook) (defvar lsp-mode-hook nil))
        (unless (boundp 'lsp-mode-map) (defvar lsp-mode-map (make-sparse-keymap)))
        (unless (fboundp 'lsp-register-client)
          (defalias 'lsp-register-client (lambda (&rest _) nil)))
        (unless (fboundp 'make-lsp-client)
          (defalias 'make-lsp-client (lambda (&rest _) nil)))
        (unless (fboundp 'lsp-stdio-connection)
          (defalias 'lsp-stdio-connection (lambda (&rest _) nil)))
        (unless (fboundp 'lsp-activate-on)
          (defalias 'lsp-activate-on (lambda (&rest _) nil)))
        (tilde-pctx--load-module "lsp")
        (should-not (memq 'lsp-go-install-save-hooks go-mode-hook))
        (should-not (fboundp 'lsp-go-install-save-hooks)))
    (tilde-pctx--remove-stubs)))

(ert-deftest tilde-pctx/development-has-no-yarn-prettier-shell ()
  "The Apheleia config must not hardcode `yarn' / `npx' as the prettier
command driver."
  (let ((body (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "development.el" tilde-pctx--modules-dir))
                (buffer-string))))
    (should-not (string-match-p "\"yarn\"[ \t\n]*\"prettier\"" body))
    (should-not (string-match-p "\"npx\"" body))
    ;; And it does reference the shared resolver.
    (should (string-match-p "my/apheleia-prettier-arg1" body))
    (should (string-match-p "my/apheleia-go-arg1" body))))

(ert-deftest tilde-pctx/prettier-arg1-prefers-node-modules ()
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--load-module "development")
        (tilde-pctx--with-tree root
          (tilde-pctx--touch (expand-file-name "package.json" root))
          (tilde-pctx--touch (expand-file-name "pnpm-lock.yaml" root))
          (tilde-pctx--mkexe
           (expand-file-name "node_modules/.bin/prettier" root))
          (let ((file (expand-file-name "src/a.ts" root)))
            (tilde-pctx--touch file)
            (with-current-buffer (generate-new-buffer " *pctx-prettier*")
              (unwind-protect
                  (progn
                    (setq buffer-file-name file)
                    (tilde-pctx--with-root root
                      (should (equal (my/apheleia-prettier-arg1)
                                     (expand-file-name
                                      "node_modules/.bin/prettier" root)))))
                (kill-buffer))))))
    (tilde-pctx--remove-stubs)))

(ert-deftest tilde-pctx/prettier-arg1-missing-gives-install-hint ()
  "With no local prettier and none on exec-path, the resolver returns
the literal `prettier' (so Apheleia's own `executable-find' guard
fires) and messages an install-locally hint -- never falls through
to npx."
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--load-module "development")
        (tilde-pctx--with-tree root
          (tilde-pctx--touch (expand-file-name "package.json" root))
          (tilde-pctx--touch (expand-file-name "pnpm-lock.yaml" root))
          (let ((file (expand-file-name "src/a.ts" root))
                (msg nil))
            (tilde-pctx--touch file)
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (setq msg (apply #'format fmt args))))
                      ((symbol-function 'executable-find)
                       (lambda (&rest _) nil)))
              (with-current-buffer (generate-new-buffer " *pctx-none*")
                (unwind-protect
                    (progn
                      (setq buffer-file-name file)
                      (tilde-pctx--with-root root
                        (should (equal (my/apheleia-prettier-arg1)
                                       "prettier"))
                        (should (stringp msg))
                        (should (string-match-p "pnpm add -D prettier" msg))))
                  (kill-buffer)))))))
    (tilde-pctx--remove-stubs)))

(ert-deftest tilde-pctx/go-arg1-prefers-goimports ()
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--load-module "development")
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (name &rest _)
                     (and (equal name "goimports") "/fake/goimports"))))
          (should (equal (my/apheleia-go-arg1) "/fake/goimports")))
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (&rest _) nil)))
          (should (equal (my/apheleia-go-arg1) "gofmt"))))
    (tilde-pctx--remove-stubs)))

(ert-deftest tilde-pctx/prettier-arg1-uses-hoisted-root-when-nested-node-modules-lacks-it ()
  "Nested workspace with its own `node_modules/' but WITHOUT prettier
must still pick up the hoisted root prettier. The old resolver
stopped at the first `node_modules/' and missed the hoisted binary."
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--load-module "development")
        (tilde-pctx--with-tree root
          (tilde-pctx--touch (expand-file-name "pnpm-lock.yaml" root))
          (tilde-pctx--touch (expand-file-name "package.json" root))
          (tilde-pctx--mkexe
           (expand-file-name "node_modules/.bin/prettier" root))
          ;; Nested workspace: has its own package.json, its own
          ;; node_modules/ (say for one dependency), but does NOT
          ;; install prettier locally.
          (let* ((inner (file-name-as-directory
                         (expand-file-name "packages/app" root)))
                 (file (expand-file-name "src/a.ts" inner)))
            (tilde-pctx--touch (expand-file-name "package.json" inner))
            (make-directory (expand-file-name "node_modules/.bin" inner) t)
            (tilde-pctx--touch file)
            (with-current-buffer (generate-new-buffer " *pctx-hoist*")
              (unwind-protect
                  (progn
                    (setq buffer-file-name file)
                    (tilde-pctx--with-root root
                      (should (equal (my/apheleia-prettier-arg1)
                                     (expand-file-name
                                      "node_modules/.bin/prettier" root)))))
                (kill-buffer))))))
    (tilde-pctx--remove-stubs)))

(ert-deftest tilde-pctx/prettier-arg1-json-buffer-picks-local-binary ()
  "JSON is a formatter asset, NOT a test language: `my/proj-context'
returns `:language nil' for `foo.json'. The prettier resolver must
still find `node_modules/.bin/prettier' by starting the search from
the file's own directory rather than giving up because there is no
`:root'."
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--load-module "development")
        (tilde-pctx--with-tree root
          (tilde-pctx--touch (expand-file-name "package.json" root))
          (tilde-pctx--touch (expand-file-name "pnpm-lock.yaml" root))
          (tilde-pctx--mkexe
           (expand-file-name "node_modules/.bin/prettier" root))
          (let ((file (expand-file-name "config/tsconfig.json" root)))
            (tilde-pctx--touch file)
            (with-current-buffer (generate-new-buffer " *pctx-json*")
              (unwind-protect
                  (progn
                    (setq buffer-file-name file)
                    (tilde-pctx--with-root root
                      ;; Resolver classifies JSON as no language (asset).
                      (should (null (plist-get (my/proj-context file)
                                               :language)))
                      ;; But prettier resolution still finds the local
                      ;; binary via the file's own directory.
                      (should (equal (my/apheleia-prettier-arg1)
                                     (expand-file-name
                                      "node_modules/.bin/prettier" root)))))
                (kill-buffer))))))
    (tilde-pctx--remove-stubs)))

;; --------------------------------------------------------------------
;; Fail-closed kind detection and scope-root consistency
;; --------------------------------------------------------------------

(ert-deftest tilde-pctx/workflow-kind-fails-closed-on-unknown-extension ()
  "A `.txt' buffer inside a repo carrying `pyproject.toml' must NOT
silently become `:python' via marker inference. Marker inference
is reserved for buffers with no file at all."
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--load-module "workflow")
        (tilde-pctx--with-tree root
          (tilde-pctx--touch (expand-file-name "pyproject.toml" root))
          (let ((file (expand-file-name "NOTES.txt" root)))
            (tilde-pctx--touch file)
            (with-current-buffer (generate-new-buffer " *pctx-txt*")
              (unwind-protect
                  (progn
                    (setq buffer-file-name file)
                    (tilde-pctx--with-root root
                      (should-error (my/workflow-run-project-tests)
                                    :type 'user-error)
                      (should-error (my/workflow-run-file-tests)
                                    :type 'user-error)))
                (kill-buffer))))))
    (tilde-pctx--remove-stubs)))

(ert-deftest tilde-pctx/workflow-nested-workspace-project-scope-uses-inner-root ()
  "Project-scope tests in a nested workspace must run at the inner
package.json directory, not at the outer VC root. The old code
picked pnpm from the buffer context yet ran the command at the
VC root, which is the wrong cwd for pnpm workspace commands."
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--load-module "workflow")
        (tilde-pctx--with-tree root
          (tilde-pctx--touch (expand-file-name "pnpm-lock.yaml" root))
          (let* ((inner (file-name-as-directory
                         (expand-file-name "packages/app" root)))
                 (file (expand-file-name "src/x.test.ts" inner)))
            (tilde-pctx--touch (expand-file-name "package.json" inner))
            (tilde-pctx--touch file)
            (with-current-buffer (generate-new-buffer " *pctx-proj*")
              (unwind-protect
                  (progn
                    (setq buffer-file-name file)
                    (tilde-pctx--with-root root
                      (my/workflow-run-project-tests)
                      (should (equal (cdar tilde-pctx--compile-calls)
                                     "pnpm test"))
                      (should (equal (caar tilde-pctx--compile-calls)
                                     inner))))
                (kill-buffer))))))
    (tilde-pctx--remove-stubs)))

(ert-deftest tilde-pctx/workflow-rerun-matches-file-scope-root ()
  "Rerun stored the last command under the scope root the file scope
used (inner workspace). Rerun must resolve the SAME root, not the
outer VC root."
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--load-module "workflow")
        (tilde-pctx--with-tree root
          (tilde-pctx--touch (expand-file-name "pnpm-lock.yaml" root))
          (let* ((inner (file-name-as-directory
                         (expand-file-name "packages/app" root)))
                 (file (expand-file-name "src/x.test.ts" inner)))
            (tilde-pctx--touch (expand-file-name "package.json" inner))
            (tilde-pctx--touch file)
            (with-current-buffer (generate-new-buffer " *pctx-rerun*")
              (unwind-protect
                  (progn
                    (setq buffer-file-name file)
                    (tilde-pctx--with-root root
                      (my/workflow-run-file-tests)
                      (let ((first (cdar tilde-pctx--compile-calls)))
                        (should (string-prefix-p "pnpm test -- " first))
                        (setq tilde-pctx--compile-calls nil)
                        (my/workflow-rerun-last-test)
                        (should (equal (cdar tilde-pctx--compile-calls)
                                       first))
                        (should (equal (caar tilde-pctx--compile-calls)
                                       inner)))))
                (kill-buffer))))))
    (tilde-pctx--remove-stubs)))

;; --------------------------------------------------------------------
;; Nested Python venv (environments.el) and hoisted TS server
;; (typescript.el) via the shared context
;; --------------------------------------------------------------------

(ert-deftest tilde-pctx/venv-project-root-uses-nested-python-context ()
  "A Python file inside a nested subproject must resolve to that
subproject's `.venv', not to the outermost VC root. Non-Python
buffers still fall back to `project-current' so scratch buffers
and shell buffers keep the pre-refactor behaviour."
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--load-module "environments")
        (tilde-pctx--with-tree root
          ;; Outer VC root has no Python markers of its own.
          (let* ((inner (file-name-as-directory
                         (expand-file-name "services/api" root)))
                 (file (expand-file-name "app/main.py" inner)))
            (tilde-pctx--touch (expand-file-name "pyproject.toml" inner))
            (tilde-pctx--touch file)
            (with-current-buffer (generate-new-buffer " *pctx-venv-py*")
              (unwind-protect
                  (progn
                    (setq buffer-file-name file)
                    (tilde-pctx--with-root root
                      (should (equal (my/venv--project-root-or-nil)
                                     inner))))
                (kill-buffer)))
            ;; Bufferless / non-Python fallback keeps returning the
            ;; outermost VC root; nothing else is available.
            (with-current-buffer (generate-new-buffer " *pctx-venv-none*")
              (unwind-protect
                  (cl-letf (((symbol-function 'project-current)
                             (lambda (&optional _) (cons 'transient root))))
                    (cl-letf (((symbol-function 'project-root)
                               (lambda (_) root)))
                      (should (equal (my/venv--project-root-or-nil)
                                     root))))
                (kill-buffer))))))
    (tilde-pctx--remove-stubs)))

(ert-deftest tilde-pctx/typescript-server-picks-hoisted-root-binary ()
  "`my/typescript-project-server' must find a hoisted root binary
when the nested workspace ships a `node_modules/' without the
typescript-language-server."
  (tilde-pctx--install-stubs)
  (unwind-protect
      (progn
        (tilde-pctx--load-module "proj-context")
        (tilde-pctx--load-module "typescript")
        (tilde-pctx--with-tree root
          (tilde-pctx--touch (expand-file-name "pnpm-lock.yaml" root))
          (tilde-pctx--mkexe
           (expand-file-name "node_modules/.bin/typescript-language-server"
                             root))
          (let* ((inner (file-name-as-directory
                         (expand-file-name "packages/app" root)))
                 (file (expand-file-name "src/a.ts" inner)))
            (tilde-pctx--touch (expand-file-name "package.json" inner))
            (make-directory (expand-file-name "node_modules/.bin" inner) t)
            (tilde-pctx--touch file)
            (with-current-buffer (generate-new-buffer " *pctx-tsls*")
              (unwind-protect
                  (progn
                    (setq buffer-file-name file)
                    (tilde-pctx--with-root root
                      (should (equal (my/typescript-project-server)
                                     (expand-file-name
                                      "node_modules/.bin/typescript-language-server"
                                      root)))))
                (kill-buffer))))))
    (tilde-pctx--remove-stubs)))

(ert-deftest tilde-pctx/boundaries-are-canonical-and-fail-closed ()
  (tilde-pctx--load-module "proj-context")
  (tilde-pctx--with-tree root
    (let ((alias (expand-file-name "alias" root))
          (inner (expand-file-name "repo/sub" root))
          (calls nil))
      (make-directory inner t)
      (make-symbolic-link inner alias)
      (should (equal (my/proj--walk-upwards
                      alias (expand-file-name "repo" root)
                      (lambda (dir) (push dir calls) nil)) nil))
      (should (equal calls (list (concat root "repo/") (concat inner "/"))))
      (setq calls nil)
      (should-error
       (my/proj--walk-upwards inner (expand-file-name "unrelated" root)
                             (lambda (dir) (push dir calls)))
       :type 'user-error)
      (should-not calls))))

(provide 'emacs-proj-context)
;;; emacs-proj-context.el ends here
