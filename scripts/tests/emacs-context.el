;;; emacs-context.el --- ERT tests for the context module -*- lexical-binding: t; -*-
;;
;; Covers persistence and navigation *mechanics* only:
;;
;;   * savehist round-trips a real minibuffer history and never writes
;;     the excluded `shell-command-history';
;;   * recentf round-trips through save/reset/load;
;;   * global-auto-revert preserves a modified buffer and refreshes an
;;     unmodified one -- real `auto-revert-handler', real disk writes;
;;   * `context.el' reloads idempotently: modes stay on, no doubled
;;     timers or hooks;
;;   * `my/workflow--run' binds `compilation-buffer-name-function' to
;;     a per-root closure so two projects do not collide;
;;   * built-in `project-shell' reuses one buffer per project root
;;     with ONLY the process boundary (`make-comint-in-buffer' and
;;     `comint-check-proc') mocked -- the real `project-shell', real
;;     `shell', and real buffer/window transitions run.
;;
;; These tests measure the mechanics above. They do not measure a
;; reduction in the user's day-to-day context switching.
;;
;; Run:
;;   emacs -Q --batch -l scripts/tests/emacs-context.el \
;;         -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'project)
(require 'comint)
(require 'shell)

(defconst tilde-context--repo-root
  (expand-file-name "../.."
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defconst tilde-context--modules-dir
  (expand-file-name "emacs/.emacs.d/modules" tilde-context--repo-root))

(add-to-list 'load-path tilde-context--modules-dir)

;; Declare context.el's dynamic vars up front so `let'-binding them
;; from tests under `lexical-binding: t' stays dynamic when the module
;; later `defvar's the same names.
(defvar my/context--state-dir)
(defvar my/context--history-file)
(defvar my/context--recentf-file)

(defun tilde-context--load ()
  (load (expand-file-name "context.el" tilde-context--modules-dir)
        nil t t))

(defun tilde-context--load-workflow ()
  (load (expand-file-name "workflow.el" tilde-context--modules-dir)
        nil t t))

(defmacro tilde-context--with-home (home &rest body)
  "Bind a fresh HOME + `user-emacs-directory' for BODY.
Modes with `kill-emacs-hook' writers are uninstalled before the
tmp dir vanishes, so batch exit is silent."
  (declare (indent 1) (debug (symbolp body)))
  `(let* ((,home (file-name-as-directory
                  (make-temp-file "tilde-context-" t)))
          (user-emacs-directory (expand-file-name ".emacs.d/" ,home))
          (my/context--state-dir
           (expand-file-name "state/" user-emacs-directory))
          (my/context--history-file
           (expand-file-name "history" my/context--state-dir))
          (my/context--recentf-file
           (expand-file-name "recentf" my/context--state-dir))
          (process-environment (copy-sequence process-environment)))
     (setenv "HOME" ,home)
     (make-directory user-emacs-directory t)
     (unwind-protect (progn ,@body)
       (when (bound-and-true-p savehist-mode) (savehist-mode -1))
       (when (bound-and-true-p recentf-mode) (recentf-mode -1))
       (when (bound-and-true-p global-auto-revert-mode)
         (global-auto-revert-mode -1))
       (when (file-directory-p ,home) (delete-directory ,home t)))))

(defun tilde-context--touch (path body)
  (with-temp-file path (insert body)))

;; --------------------------------------------------------------------
;; Savehist / recentf persistence
;; --------------------------------------------------------------------

(ert-deftest tilde-context/savehist-persists-and-excludes ()
  "A registered history round-trips; an excluded history does not."
  (tilde-context--with-home _home
    (let ((savehist-mode nil) (savehist-loaded nil)
          (savehist-ignored-variables nil)
          (savehist-additional-variables nil)
          (savehist-minibuffer-history-variables nil)
          (savehist-file nil)
          (extended-command-history nil) (shell-command-history nil)
          (read-expression-history nil))
      (tilde-context--load)
      (dolist (sym '(kill-ring register-alist
                               shell-command-history command-history
                               read-expression-history eval-expression-history))
        (should (memq sym savehist-ignored-variables)))
      (should (file-exists-p savehist-file))
      (should (= #o600 (file-modes savehist-file)))
      (should (= #o700 (file-modes my/context--state-dir)))
      ;; Register two minibuffer-history variables (as
      ;; `savehist-minibuffer-hook' would); only one should persist.
      (setq savehist-minibuffer-history-variables
            '(extended-command-history shell-command-history read-expression-history)
            extended-command-history '("first-command" "second-command")
            shell-command-history '("private shell input")
            read-expression-history '("private expression input"))
      (savehist-save)
      (setq extended-command-history nil shell-command-history nil
            read-expression-history nil savehist-minibuffer-history-variables nil)
      (load savehist-file nil t t)
      (should (equal extended-command-history
                     '("first-command" "second-command")))
      (should (null shell-command-history))
      (should (null read-expression-history)))))

(ert-deftest tilde-context/recentf-persists-local-paths ()
  "A local file added to `recentf-list' round-trips through save/load."
  (tilde-context--with-home home
    (let ((recentf-mode nil) (recentf-list nil)
          (recentf-save-file nil) (recentf-auto-cleanup 'mode))
      (tilde-context--load)
      (should (eq recentf-auto-cleanup 'never))
      (should (= #o600 (file-modes recentf-save-file)))
      (let ((file (expand-file-name "hello.txt" home)))
        (tilde-context--touch file "hi\n")
        (setq recentf-list (list file))
        (recentf-save-list)
        (setq recentf-list nil)
        (recentf-load-list)
        (should (member file recentf-list))))))

;; --------------------------------------------------------------------
;; global-auto-revert
;; --------------------------------------------------------------------

(ert-deftest tilde-context/auto-revert-preserves-modified-and-refreshes-clean ()
  "Modified buffers keep their edits; unmodified buffers reload."
  (tilde-context--with-home home
    (tilde-context--load)
    (should (eq auto-revert-remote-files nil))
    (should (eq global-auto-revert-non-file-buffers nil))
    (let* ((file-a (expand-file-name "keep.txt" home))
           (file-b (expand-file-name "refresh.txt" home))
           buf-a buf-b)
      (tilde-context--touch file-a "original\n")
      (tilde-context--touch file-b "original\n")
      (setq buf-a (find-file-noselect file-a)
            buf-b (find-file-noselect file-b))
      (unwind-protect
          (progn
            (with-current-buffer buf-a
              (goto-char (point-max))
              (insert "unsaved local change\n")
              (should (buffer-modified-p))
              (sleep-for 0.01)
              (tilde-context--touch file-a "external change\n")
              (auto-revert-handler)
              (should (buffer-modified-p))
              (should (string-match-p "unsaved local change"
                                      (buffer-string)))
              (should-not (string-match-p "external change"
                                          (buffer-string))))
            (with-current-buffer buf-b
              (should-not (buffer-modified-p))
              (sleep-for 0.01)
              (tilde-context--touch file-b "reloaded content\n")
              (auto-revert-handler)
              (should (string-match-p "reloaded content"
                                      (buffer-string)))))
        (with-current-buffer buf-a (set-buffer-modified-p nil))
        (kill-buffer buf-a)
        (kill-buffer buf-b)))))

;; --------------------------------------------------------------------
;; Idempotent reload
;; --------------------------------------------------------------------

(ert-deftest tilde-context/module-reload-is-idempotent ()
  "Loading the module twice keeps modes on without doubling state."
  (tilde-context--with-home _home
    (tilde-context--load)
    (let ((first-timer savehist-timer))
      (tilde-context--load)
      (should savehist-mode)
      (should recentf-mode)
      (should global-auto-revert-mode)
      (should (eq savehist-timer first-timer))
      (should (= 1 (cl-count 'savehist-minibuffer-hook
                             minibuffer-setup-hook)))
      (should (= 1 (cl-count 'savehist-autosave kill-emacs-hook))))))

;; --------------------------------------------------------------------
;; Project-scoped compilation buffer names
;; --------------------------------------------------------------------

(defvar tilde-context--compile-buffers nil
  "List of (root . (cmd . buffer-name)) captured from stubbed compile.")

(defun tilde-context--install-workflow-stubs ()
  (setq tilde-context--compile-buffers nil)
  (advice-add 'compile :override
              (lambda (cmd &rest _)
                (let ((name (funcall compilation-buffer-name-function
                                     "compilation")))
                  (push (cons default-directory (cons cmd name))
                        tilde-context--compile-buffers)))
              '((name . tilde-context-compile)))
  (unless (get 'package-installed-p 'tilde-context-stubbed)
    (advice-add 'package-installed-p :override (lambda (&rest _) nil)
                '((name . tilde-context-pkg)))
    (put 'package-installed-p 'tilde-context-stubbed t)))

(defun tilde-context--remove-workflow-stubs ()
  (advice-remove 'compile 'tilde-context-compile)
  (advice-remove 'package-installed-p 'tilde-context-pkg)
  (put 'package-installed-p 'tilde-context-stubbed nil))

(ert-deftest tilde-context/compile-buffer-name-is-project-scoped ()
  "Two roots produce two distinct compile buffer names."
  (tilde-context--install-workflow-stubs)
  (unwind-protect
      (progn
        (tilde-context--load-workflow)
        (let* ((root-a (file-name-as-directory
                        (make-temp-file "tilde-ctx-proj-alpha-" t)))
               (root-b (file-name-as-directory
                        (make-temp-file "tilde-ctx-proj-beta-" t))))
          (unwind-protect
              (progn
                (tilde-context--touch
                 (expand-file-name "pyproject.toml" root-a) "")
                (tilde-context--touch
                 (expand-file-name "pyproject.toml" root-b) "")
                (cl-letf (((symbol-function 'my/workflow--project-root)
                           (lambda () root-a))
                          ((symbol-function 'my/proj--vcs-root)
                           (lambda (&rest _) root-a)))
                  (my/workflow-run-project-tests))
                (cl-letf (((symbol-function 'my/workflow--project-root)
                           (lambda () root-b))
                          ((symbol-function 'my/proj--vcs-root)
                           (lambda (&rest _) root-b)))
                  (my/workflow-run-project-tests))
                (should (= 2 (length tilde-context--compile-buffers)))
                (let* ((names (mapcar #'cddr tilde-context--compile-buffers))
                       (name-a (my/workflow--compile-buffer-name root-a))
                       (name-b (my/workflow--compile-buffer-name root-b)))
                  (should (member name-a names))
                  (should (member name-b names))
                  (should-not (equal name-a name-b))
                  (should-not (equal
                               (my/workflow--compile-buffer-name (expand-file-name "app" root-a))
                               (my/workflow--compile-buffer-name (expand-file-name "app" root-b))))
                  (should (string-match-p "\\`\\*compilation:.+\\*\\'"
                                          name-a))))
            (dolist (r (list root-a root-b))
              (when (file-directory-p r) (delete-directory r t))))))
    (tilde-context--remove-workflow-stubs)))

;; --------------------------------------------------------------------
;; Built-in project-shell reuse (process boundary mocked)
;; --------------------------------------------------------------------

(defvar tilde-context--shell-started nil
  "Names of buffers whose process boundary we faked live.")

(defun tilde-context--install-shell-stubs ()
  (setq tilde-context--shell-started nil)
  (advice-add 'make-comint-in-buffer :override
              (lambda (_name buffer &rest _)
                (push (buffer-name (get-buffer-create buffer))
                      tilde-context--shell-started)
                buffer)
              '((name . tilde-context-mcib)))
  (advice-add 'comint-check-proc :override
              (lambda (buffer)
                (member (if (bufferp buffer) (buffer-name buffer) buffer)
                        tilde-context--shell-started))
              '((name . tilde-context-ccp))))

(defun tilde-context--remove-shell-stubs ()
  (advice-remove 'make-comint-in-buffer 'tilde-context-mcib)
  (advice-remove 'comint-check-proc 'tilde-context-ccp))

(cl-defstruct tilde-context--fake-project root)
(cl-defmethod project-root ((p tilde-context--fake-project))
  (tilde-context--fake-project-root p))
(cl-defmethod project-name ((p tilde-context--fake-project))
  (file-name-nondirectory
   (directory-file-name (tilde-context--fake-project-root p))))

(ert-deftest tilde-context/project-shell-reuses-per-root ()
  "Reuse stays at the same root, even when both projects are named app."
  (tilde-context--with-home _home
    (tilde-context--load)
    (tilde-context--install-shell-stubs)
    (unwind-protect
	(let* ((home (make-temp-file "tilde-ctx-shell-" t))
               (root-a (expand-file-name "a/app/" home))
               (root-b (expand-file-name "b/app/" home))
               (proj-a (make-tilde-context--fake-project :root root-a))
               (proj-b (make-tilde-context--fake-project :root root-b))
               (current-proj proj-a)
               (original-pop (symbol-function 'pop-to-buffer))
               (opened nil))
          (make-directory root-a t)
          (make-directory root-b t)
          (unwind-protect
              (cl-letf (((symbol-function 'project-current)
			 (lambda (&rest _) current-proj))
			((symbol-function 'pop-to-buffer)
			 (lambda (buffer &rest args)
                           (prog1 (apply original-pop buffer args)
                             (push (get-buffer buffer) opened)))))
		(project-shell)
		(let ((first-a (car opened)))
                  (should (buffer-live-p first-a))
                  (should (eq (window-buffer (selected-window)) first-a))
                  (project-shell)
                  (should (eq first-a (car opened)))
                  (setq current-proj proj-b)
                  (project-shell)
                  (let ((first-b (car opened)))
                    (should-not (eq first-a first-b))
                    (should (string-match-p
                             (regexp-quote
                              (file-name-nondirectory
                               (directory-file-name root-a)))
                             (buffer-name first-a)))
                    (should (string-match-p
                             (regexp-quote
                              (file-name-nondirectory
                               (directory-file-name root-b)))
                             (buffer-name first-b))))))
            (dolist (buf opened)
              (when (buffer-live-p buf) (kill-buffer buf)))
            (delete-directory home t)))
      (tilde-context--remove-shell-stubs))))

;;; emacs-context.el ends here
