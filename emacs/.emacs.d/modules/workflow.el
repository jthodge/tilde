;;; workflow.el --- Focused project workflow pilot -*- lexical-binding: t; -*-
;;
;; PILOT: opt-in ergonomics for project navigation, git, search, tests.
;; Conventional keys under `C-c'; no Evil; nothing runs on file open.
;;
;;   C-c p  built-in `project-prefix-map'
;;   C-c g  sparse Magit map (s status, l log, b blame) - autoloads only
;;   C-c s  Consult search map (l/L line, r ripgrep, g grep, f find, i/o)
;;   C-c t  Project tests: p project, f file, n nearest, r rerun
;;   C-.    `embark-act'  (only if unbound globally)
;;   C-;    `embark-dwim' (only if unbound globally)
;;
;; This module never disables Magit's signing, push-confirmation, or
;; unsafe-action prompts. Test commands go through `compile' with
;; `default-directory' pinned to the SAME resolved scope root for
;; all three scopes (project / file / nearest) and for rerun. That
;; scope root is the language-specific `:root' from `proj-context'
;; when a buffer file resolves one, else the outermost VC root when
;; the buffer has no file. Using one root per (buffer-file, project)
;; keeps the per-project last-command hash consistent across scopes:
;; running file tests then rerun no longer looks up an inner root
;; while the entry was stored under an outer root.

(require 'project)
(require 'compile)
(require 'seq)
(require 'subr-x)
(require 'proj-context)

(define-key global-map (kbd "C-c p") project-prefix-map)

(defvar my/workflow-magit-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "s") #'magit-status)
    (define-key m (kbd "l") #'magit-log-current)
    (define-key m (kbd "b") #'magit-blame-addition)
    m)
  "Sparse Magit entry map bound at `C-c g' (autoloaded targets).")
(define-key global-map (kbd "C-c g") my/workflow-magit-map)

(defvar my/workflow-consult-search-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "l") #'consult-line)
    (define-key m (kbd "L") #'consult-line-multi)
    (define-key m (kbd "r") #'consult-ripgrep)
    (define-key m (kbd "g") #'consult-grep)
    (define-key m (kbd "f") #'consult-find)
    (define-key m (kbd "i") #'consult-imenu)
    (define-key m (kbd "o") #'consult-outline)
    m)
  "Sparse Consult search prefix map bound at `C-c s'.")
(define-key global-map (kbd "C-c s") my/workflow-consult-search-map)

;; Embark: bind only when the global slot is free, to avoid shadowing.
(when (package-installed-p 'embark)
  (unless (global-key-binding (kbd "C-."))
    (global-set-key (kbd "C-.") #'embark-act))
  (unless (global-key-binding (kbd "C-;"))
    (global-set-key (kbd "C-;") #'embark-dwim)))
(when (package-installed-p 'embark-consult)
  (with-eval-after-load 'embark
    (with-eval-after-load 'consult
      (require 'embark-consult nil t))))

;; which-key is declared elsewhere; enable it here so the prefix maps
;; above are immediately discoverable.
(when (package-installed-p 'which-key)
  (require 'which-key nil t)
  (when (fboundp 'which-key-mode) (which-key-mode 1)))

(defvar my/workflow--last-test (make-hash-table :test 'equal)
  "Per-project last test command, keyed by absolute project root string.
Rerun stays scoped to one repo; other repos keep their own history.")

;; Per-scope overrides. All three are buffer-local and deliberately
;; NOT marked with `safe-local-variable', so a dir-local or file-local
;; assignment prompts on first read (they are free-form shell strings
;; from an untrusted checkout). Each variable applies only to the
;; scope named in its symbol; there is no cascade.
(defvar-local my/workflow-project-test-command nil
  "Buffer-local override for `my/workflow-run-project-tests'.")
(defvar-local my/workflow-file-test-command nil
  "Buffer-local override for `my/workflow-run-file-tests'.")
(defvar-local my/workflow-nearest-test-command nil
  "Buffer-local override for `my/workflow-run-nearest-test'.")

(defun my/workflow--project-root ()
  "Return current project's absolute VC root, or `user-error' if none."
  (let ((proj (project-current nil)))
    (unless proj (user-error "No current project"))
    (my/proj--norm (project-root proj))))

(defun my/workflow--detect-from-markers (root)
  "Detect the language of ROOT purely from marker files.
Used only when the current buffer has no file (so file-extension
detection is unavailable). Ambiguous multi-language roots signal
`user-error' pointing the user at the project override -- we
refuse to guess Python just because pyproject.toml exists next
to package.json and go.mod."
  (let ((found nil))
    (when (seq-some (lambda (m) (file-exists-p (expand-file-name m root)))
                    my/proj--python-markers)
      (push :python found))
    (when (file-exists-p (expand-file-name "go.mod" root))
      (push :go found))
    (when (file-exists-p (expand-file-name "package.json" root))
      (push :js found))
    (cond
     ((null found) nil)
     ((cdr found)
      (user-error
       "Multiple project languages at %s (%s); open a source file or set `my/workflow-project-test-command'"
       root
       (mapconcat (lambda (k) (substring (symbol-name k) 1))
                  (nreverse found) ",")))
     (t (car found)))))

(defun my/workflow--kind ()
  "Return the language keyword for the current buffer's test target.

Fails closed. When `buffer-file-name' is non-nil, the language is
resolved purely by `my/proj-context' from the file's extension --
an unknown extension (`.txt', `.rst', ...) raises `user-error'
rather than sliding into project-marker inference. Only when the
buffer has no file at all does the resolver look at unambiguous
marker files at the project root."
  (let* ((file buffer-file-name)
         (ctx (my/proj-context file))
         (lang (plist-get ctx :language)))
    (cond
     (lang lang)
     (file
      (user-error
       "Unsupported file type for %s (no language detected from extension)"
       file))
     (t
      (or (my/workflow--detect-from-markers (my/workflow--project-root))
          (user-error
           "Unsupported project type at %s (open a source file or set `my/workflow-project-test-command')"
           (my/workflow--project-root)))))))

(defun my/workflow--scope-root ()
  "Return the compile working directory for the current buffer.
All three scopes (project / file / nearest) AND rerun call this
helper, so the per-project last-command hash uses exactly one key
for a given (buffer-file, project). Prefer the language-specific
`:root' from `my/proj-context' when a buffer file resolves one;
else fall back to the outermost VC project root."
  (let* ((ctx (my/proj-context buffer-file-name))
         (r (plist-get ctx :root)))
    (or r (my/workflow--project-root))))

(defun my/workflow--js-runner-string (root)
  "Return \"pnpm\" / \"yarn\" / \"npm\" for ROOT, or `user-error'.
Consults the current buffer's context first (nearest lockfile at
or above the nearest package.json, bounded by the repo). Falls
back to a direct search from ROOT (bounded at ROOT itself) when
the buffer has no file and no VC-root boundary is available."
  (let* ((ctx (my/proj-context buffer-file-name))
         (pm (or (plist-get ctx :package-manager)
                 (my/proj--js-runner
                  root (or (plist-get ctx :vcs-root) root)))))
    (pcase pm
      (:pnpm "pnpm")
      (:yarn "yarn")
      (:npm  "npm")
      (_ (user-error "No JS lockfile at or above %s" root)))))

(defun my/workflow--project-cmd (root kind)
  "Return the whole-project test command string for ROOT of KIND."
  (pcase kind
    (:python "uv run pytest")
    (:go     "go test ./...")
    ((or :js :ts)
     (format "%s test" (my/workflow--js-runner-string root)))
    (_ (user-error "Unsupported project type at %s" root))))

(defun my/workflow--file-cmd (root kind file)
  "Return the file-scoped test command string for KIND at ROOT.
FILE is passed through `shell-quote-argument'."
  (let ((qf (shell-quote-argument file)))
    (pcase kind
      (:python (format "uv run pytest %s" qf))
      (:go (format "go test %s"
                   (shell-quote-argument
                    (concat "./" (file-relative-name
                                  (file-name-directory file) root)))))
      ((or :js :ts)
       (format "%s test -- %s"
               (my/workflow--js-runner-string root) qf))
      (_ (user-error "Unsupported project type at %s" root)))))

(defun my/workflow--python-nearest (file)
  "Build an exact pytest node ID for the enclosing `test_*' function."
  (require 'python)
  (let ((defun (python-info-current-defun)))
    (unless (and defun
                 (string-prefix-p "test"
                                  (car (last (split-string defun "\\.")))))
      (user-error "Point is not in a Python test function"))
    (format "uv run pytest %s"
            (shell-quote-argument
             (concat file "::" (string-join (split-string defun "\\.") "::"))))))

(defun my/workflow--go-nearest (root file)
  "Build `go test -run ^TestX$ ./pkg' from the nearest preceding func."
  (save-excursion
    (beginning-of-line)
    (unless (or (looking-at "^func\\b")
                (re-search-backward "^func\\b" nil t))
      (user-error "No preceding Go function"))
    (unless (looking-at "func[ \t]+\\(Test[[:alnum:]_]+\\)[ \t]*(")
      (user-error "Point is not in a Go Test function"))
    (format "go test -run %s %s"
            (shell-quote-argument
             (format "^%s$" (match-string-no-properties 1)))
            (shell-quote-argument
             (concat "./" (file-relative-name
                           (file-name-directory file) root))))))

(defun my/workflow--nearest-cmd (root kind file)
  (pcase kind
    (:python (my/workflow--python-nearest file))
    (:go (my/workflow--go-nearest root file))
    ((or :js :ts)
     (user-error
      "Nearest JS/TS test is not supported; use file tests or set `my/workflow-nearest-test-command'"))
    (_ (user-error "Unsupported project type at %s" root))))

(defun my/workflow--compile-buffer-name (root)
  "Return a distinct compilation buffer name for canonical project ROOT.
Use the full path: separate repositories can have the same basename."
  (format "*compilation:%s*"
          (abbreviate-file-name (directory-file-name (my/proj--norm root)))))

(defun my/workflow--run (cmd root)
  (puthash root cmd my/workflow--last-test)
  (let* ((default-directory root)
         (target-name (my/workflow--compile-buffer-name root))
         (compilation-buffer-name-function
          (lambda (_mode) target-name)))
    (compile cmd)))

;;;###autoload
(defun my/workflow-run-project-tests ()
  "Run the project's test suite in `compilation-mode'."
  (interactive)
  (let* ((cmd my/workflow-project-test-command)
         (root (my/workflow--scope-root)))
    (my/workflow--run
     (or cmd (my/workflow--project-cmd root (my/workflow--kind)))
     root)))

;;;###autoload
(defun my/workflow-run-file-tests ()
  "Run tests scoped to the current file."
  (interactive)
  (unless buffer-file-name (user-error "Buffer has no file"))
  (let* ((cmd my/workflow-file-test-command)
         (root (my/workflow--scope-root)))
    (my/workflow--run
     (or cmd (my/workflow--file-cmd
              root (my/workflow--kind) buffer-file-name))
     root)))

;;;###autoload
(defun my/workflow-run-nearest-test ()
  "Run the nearest test at point.
Python uses `python-info-current-defun'; Go anchors to the
enclosing `Test...' function; JS/TS explicitly errors instead of
guessing a runner."
  (interactive)
  (unless buffer-file-name (user-error "Buffer has no file"))
  (let* ((cmd my/workflow-nearest-test-command)
         (root (my/workflow--scope-root)))
    (my/workflow--run
     (or cmd (my/workflow--nearest-cmd
              root (my/workflow--kind) buffer-file-name))
     root)))

;;;###autoload
(defun my/workflow-rerun-last-test ()
  "Rerun the last test command recorded for the current scope root.
The scope root matches `my/workflow--scope-root': the language
`:root' when a buffer file resolves one, else the VC root. This
is exactly the key used by every scope that stores commands, so
rerun always finds the previous entry."
  (interactive)
  (let* ((root (my/workflow--scope-root))
         (cmd (gethash root my/workflow--last-test)))
    (unless cmd (user-error "No previous test command for %s" root))
    (my/workflow--run cmd root)))

(defvar my/workflow-test-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "p") #'my/workflow-run-project-tests)
    (define-key m (kbd "f") #'my/workflow-run-file-tests)
    (define-key m (kbd "n") #'my/workflow-run-nearest-test)
    (define-key m (kbd "r") #'my/workflow-rerun-last-test)
    m)
  "Project test command map bound at `C-c t'.")
(define-key global-map (kbd "C-c t") my/workflow-test-map)

(provide 'workflow)
;;; workflow.el ends here
