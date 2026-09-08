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
;; `default-directory' pinned to the project root.

(require 'project)
(require 'subr-x)

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

(defvar-local my/workflow-project-test-command nil
  "Buffer-local override for the project-wide test command.
Deliberately NOT auto-marked safe: `safe-local-variable' is unset so
Emacs prompts before applying a dir-local or file-local value.")

(defun my/workflow--project-root ()
  "Return current project's absolute root, or `user-error' if none."
  (let ((proj (project-current nil)))
    (unless proj (user-error "No current project"))
    (expand-file-name (project-root proj))))

(defun my/workflow--detect-kind (root)
  "Return :python, :go, :js, or nil for ROOT."
  (cond
   ((or (file-exists-p (expand-file-name "pyproject.toml" root))
        (file-exists-p (expand-file-name "uv.lock" root))
        (file-exists-p (expand-file-name "setup.py" root))
        (file-exists-p (expand-file-name "requirements.txt" root)))
    :python)
   ((file-exists-p (expand-file-name "go.mod" root)) :go)
   ((file-exists-p (expand-file-name "package.json" root)) :js)))

(defun my/workflow--js-runner (root)
  "Return \"pnpm\"/\"yarn\"/\"npm\" from ROOT lockfile, else `user-error'."
  (cond ((file-exists-p (expand-file-name "pnpm-lock.yaml" root)) "pnpm")
        ((file-exists-p (expand-file-name "yarn.lock" root)) "yarn")
        ((file-exists-p (expand-file-name "package-lock.json" root)) "npm")
        (t (user-error "No JS lockfile in %s" root))))

(defun my/workflow--project-cmd (root kind)
  (pcase kind
    (:python "uv run pytest")
    (:go "go test ./...")
    (:js (format "%s test" (my/workflow--js-runner root)))
    (_ (user-error "Unsupported project type at %s" root))))

(defun my/workflow--file-cmd (root kind file)
  (let ((qf (shell-quote-argument file)))
    (pcase kind
      (:python (format "uv run pytest %s" qf))
      (:go (format "go test %s"
                   (shell-quote-argument
                    (concat "./" (file-relative-name
                                  (file-name-directory file) root)))))
      (:js (format "%s test -- %s" (my/workflow--js-runner root) qf))
      (_ (user-error "Unsupported project type at %s" root)))))

(defun my/workflow--python-nearest (file)
  (require 'python)
  (let ((defun (python-info-current-defun)))
    (unless (and defun (string-prefix-p "test" (car (last (split-string defun "\\.")))))
      (user-error "Point is not in a Python test function"))
    (format "uv run pytest %s"
            (shell-quote-argument
             (concat file "::" (string-join (split-string defun "\\.") "::"))))))

(defun my/workflow--go-nearest (root file)
  (save-excursion
    (beginning-of-line)
    (unless (or (looking-at "^func\\b") (re-search-backward "^func\\b" nil t))
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
    (:js (user-error
          "Nearest JS test is not supported; use file tests or an explicit project command"))
    (_ (user-error "Unsupported project type at %s" root))))

(defun my/workflow--run (cmd root)
  (puthash root cmd my/workflow--last-test)
  (let ((default-directory root))
    (compile cmd)))

;;;###autoload
(defun my/workflow-run-project-tests ()
  "Run the project's test suite in `compilation-mode'."
  (interactive)
  (let ((root (my/workflow--project-root)))
    (my/workflow--run
     (or my/workflow-project-test-command
         (my/workflow--project-cmd root (my/workflow--detect-kind root)))
     root)))

;;;###autoload
(defun my/workflow-run-file-tests ()
  "Run tests scoped to the current file."
  (interactive)
  (unless buffer-file-name (user-error "Buffer has no file"))
  (let ((root (my/workflow--project-root)))
    (my/workflow--run
     (my/workflow--file-cmd root (my/workflow--detect-kind root)
                            buffer-file-name)
     root)))

;;;###autoload
(defun my/workflow-run-nearest-test ()
  "Run the nearest test at point.
Python uses `python-info-current-defun'; Go anchors to the enclosing
`Test...' function; JS explicitly errors instead of guessing."
  (interactive)
  (unless buffer-file-name (user-error "Buffer has no file"))
  (let ((root (my/workflow--project-root)))
    (my/workflow--run
     (my/workflow--nearest-cmd root (my/workflow--detect-kind root)
                               buffer-file-name)
     root)))

;;;###autoload
(defun my/workflow-rerun-last-test ()
  "Rerun the last test command recorded for the current project."
  (interactive)
  (let* ((root (my/workflow--project-root))
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
