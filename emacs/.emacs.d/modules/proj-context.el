;;; proj-context.el --- Shared per-file project context resolver -*- lexical-binding: t; -*-
;;
;; Small resolver used by:
;;
;;   * `workflow.el'      -- test working dir + runner selection
;;   * `development.el'   -- Apheleia prettier / Go formatter dispatch
;;   * `environments.el'  -- Python venv root lookup
;;   * `typescript.el'    -- typescript-language-server discovery
;;
;; Given a file (or nil for the current buffer), `my/proj-context'
;; returns a plist:
;;
;;   :file            absolute path (or nil)
;;   :language        :python | :go | :ts | :js | :emacs-lisp | nil
;;   :root            nearest project root for that language (or nil)
;;   :vcs-root        outermost `project-current' root, bounds searches
;;   :package-manager :pnpm | :yarn | :npm  (JS/TS only; nil otherwise)
;;   :node-modules    absolute path to nearest node_modules/ (JS/TS only)
;;   :venv            absolute .venv path at :root (:python only)
;;
;; Rules:
;;
;;   * Language is chosen from the file's extension first, then from
;;     the buffer's major-mode when there is no file. It is NEVER
;;     inferred from a project marker: a `.ts' file in a mixed
;;     Python + JS + Go repo is `:ts', not `:python'.
;;   * All manifest / lockfile / node_modules searches walk upward
;;     from the canonical file directory and stop at `:vcs-root'.
;;     An unrelated boundary is an error, never permission to search higher.
;;   * `:root' is the nearest language-specific manifest directory.
;;   * Unknown language yields `(:language nil :root nil)'.
;;     Callers turn that into an actionable `user-error'; the
;;     resolver never guesses.
;;
;; Loading this module installs no tools and starts no language servers.
;; Resolution reads filesystem metadata and calls `project-current'; Emacs
;; project and file handlers can invoke subprocesses or remote I/O.

(require 'project)
(require 'seq)

(defconst my/proj--python-markers
  '("pyproject.toml" "setup.py" "setup.cfg" "requirements.txt" "uv.lock")
  "File names that mark a Python project root.
Order does not matter: any hit stops the upward search.")

(defconst my/proj--go-markers '("go.mod")
  "File names that mark a Go module root.")

(defconst my/proj--js-markers '("package.json")
  "File names that mark a JS/TS project root.")

(defconst my/proj--js-lockfiles
  '(("pnpm-lock.yaml"     . :pnpm)
    ("yarn.lock"          . :yarn)
    ("package-lock.json"  . :npm)
    ("npm-shrinkwrap.json" . :npm))
  "Lockfiles that pin the JS package manager.
Order controls priority when several are present in the same
directory (pnpm > yarn > npm).")

(defun my/proj--norm (path)
  "Return PATH as a canonical absolute directory, or nil."
  (and path (file-name-as-directory (file-truename (expand-file-name path)))))

(defun my/proj--vcs-root (file)
  "Return the root selected by `project-current' for FILE, or nil.
FILE may be nil, in which case `default-directory' is consulted."
  (let* ((abs (and file (expand-file-name file)))
         (dir (or (and abs (file-name-directory abs)) default-directory))
         (proj (let ((default-directory dir)) (project-current nil))))
    (and proj (my/proj--norm (project-root proj)))))

(defun my/proj--language-for-file (file)
  "Return the language keyword for FILE, or nil.
Extension wins; when FILE is nil, `major-mode' of the current
buffer is consulted as a secondary signal. Language is never
inferred from project markers -- see module commentary."
  (let ((ext (and file (downcase (or (file-name-extension file) "")))))
    (cond
     ((member ext '("py" "pyi"))               :python)
     ((member ext '("go"))                     :go)
     ((member ext '("ts" "tsx" "mts" "cts"))   :ts)
     ((member ext '("js" "jsx" "mjs" "cjs"))   :js)
     ((member ext '("el"))                     :emacs-lisp)
     ((null file)
      (pcase major-mode
        ((or 'python-mode 'python-ts-mode)          :python)
        ((or 'go-mode 'go-ts-mode)                  :go)
        ((or 'typescript-ts-mode 'tsx-ts-mode)      :ts)
        ((or 'js-ts-mode 'js-mode 'js2-mode 'js3-mode) :js)
        ('emacs-lisp-mode                           :emacs-lisp))))))

(defun my/proj--walk-upwards (start boundary hit-fn)
  "Walk from START to BOUNDARY inclusive, returning the first HIT-FN result.
Paths are canonical. Reject unrelated boundaries before inspecting any file.
A nil BOUNDARY permits walking to the filesystem root."
  (let* ((dir (my/proj--norm start))
         (limit (my/proj--norm boundary))
         (found nil))
    (when (and limit dir (not (string-prefix-p limit dir)))
      (user-error "Directory %s is outside project boundary %s" dir limit))
    (while (and dir (not found))
      (setq found (funcall hit-fn dir))
      (cond
       (found nil)
       ((and limit (string= dir limit)) (setq dir nil))
       (t
        (let ((parent (file-name-directory (directory-file-name dir))))
          (setq dir (unless (or (null parent) (string= parent dir))
                      parent))))))
    found))

(defun my/proj--find-upwards (start markers boundary)
  "Return absolute directory at/above START containing any of MARKERS.
Bounded by BOUNDARY -- see `my/proj--walk-upwards'."
  (my/proj--walk-upwards
   start boundary
   (lambda (dir)
     (and (seq-some (lambda (m) (file-exists-p (expand-file-name m dir)))
                    markers)
          dir))))

(defun my/proj--js-runner (start boundary)
  "Return the package-manager keyword for the nearest lockfile at/above START.
Search is bounded by BOUNDARY. Returns nil when no lockfile is
found within the boundary."
  (my/proj--walk-upwards
   start boundary
   (lambda (dir)
     (let (kind)
       (dolist (pair my/proj--js-lockfiles)
         (when (and (not kind)
                    (file-exists-p (expand-file-name (car pair) dir)))
           (setq kind (cdr pair))))
       kind))))

(defun my/proj--node-modules-dir (start boundary)
  "Return absolute `node_modules/' path at/above START (bounded), or nil.
Returns the first `node_modules/' encountered walking upward.
Note that this only proves the directory exists -- callers that
need a specific hoisted binary should use `my/proj-find-node-bin'
instead, which walks past `node_modules/' entries that do not
contain the wanted executable."
  (my/proj--walk-upwards
   start boundary
   (lambda (dir)
     (let ((cand (expand-file-name "node_modules" dir)))
       (and (file-directory-p cand) (file-name-as-directory cand))))))

(defun my/proj-find-node-bin (start name boundary)
  "Return absolute path to `node_modules/.bin/NAME' at/above START.
Bounded by BOUNDARY. Walks every ancestor; the first ancestor
whose `node_modules/.bin/NAME' is executable wins. This makes a
hoisted root binary reachable when the nearest workspace ships
a `node_modules/' without the requested tool. Nil when nothing
matches."
  (my/proj--walk-upwards
   start boundary
   (lambda (dir)
     (let ((cand (expand-file-name
                  (concat "node_modules/.bin/" name) dir)))
       (and (file-executable-p cand) cand)))))

(defun my/proj-context (&optional file)
  "Return the shared project context plist for FILE (or current buffer).
Path/project inspection errors propagate. Callers needing a project inspect
`:language' / `:root' and raise their own actionable `user-error'."
  (let* ((file (or file buffer-file-name))
         (lang (my/proj--language-for-file file))
         (start (or (and file (file-name-directory (expand-file-name file)))
                    default-directory))
         (vcs (my/proj--vcs-root file))
         ;; When the file lives outside any VC / project, bound the
         ;; search at "/" so we still stop somewhere sane.
         (boundary (or vcs "/"))
         (root
          (pcase lang
            (:python     (my/proj--find-upwards start my/proj--python-markers boundary))
            (:go         (my/proj--find-upwards start my/proj--go-markers     boundary))
            ((or :js :ts) (my/proj--find-upwards start my/proj--js-markers    boundary))
            (:emacs-lisp vcs)
            (_ nil)))
         (js-p (memq lang '(:js :ts))))
    (list :file        (and file (expand-file-name file))
          :language    lang
          :root        root
          :vcs-root    vcs
          :package-manager (and root js-p
                                (my/proj--js-runner root boundary))
          :node-modules    (and root js-p
                                (my/proj--node-modules-dir root boundary))
          :venv        (and root (eq lang :python)
                            (let ((v (expand-file-name ".venv" root)))
                              (and (file-directory-p v) v))))))

(provide 'proj-context)
;;; proj-context.el ends here
