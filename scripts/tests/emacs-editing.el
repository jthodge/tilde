;;; emacs-editing.el --- ERT tests for editing conveniences -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'smerge-mode)

(defconst tilde-editing--repo-root
  (expand-file-name "../.."
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defconst tilde-editing--modules-dir
  (expand-file-name "emacs/.emacs.d/modules" tilde-editing--repo-root))

(add-to-list 'load-path tilde-editing--modules-dir)

(defun tilde-editing--load ()
  (load (expand-file-name "editing.el" tilde-editing--modules-dir)
        nil t t))

(ert-deftest tilde-editing/fill-or-unfill-toggles-on-repeat ()
  (tilde-editing--load)
  (with-temp-buffer
    (let ((fill-column 12)
          (paragraph "alpha beta gamma delta epsilon"))
      (insert paragraph)
      (goto-char (point-min))
      (let ((last-command nil))
        (call-interactively #'my/fill-or-unfill)
        (should (string-match-p "\n" (buffer-string)))
        (setq last-command 'my/fill-or-unfill)
        (call-interactively #'my/fill-or-unfill)
        (should (equal (buffer-string) paragraph))))))

(ert-deftest tilde-editing/delete-current-buffer-file-confirms-and-deletes ()
  (tilde-editing--load)
  (let* ((dir (make-temp-file "tilde-editing-delete-" t))
         (file (expand-file-name "victim.txt" dir))
         buffer)
    (unwind-protect
        (progn
          (with-temp-file file (insert "delete me\n"))
          (setq buffer (find-file-noselect file))
          (with-current-buffer buffer
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (my/delete-current-buffer-file)))
          (should-not (file-exists-p file))
          (should-not (buffer-live-p buffer)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when (file-directory-p dir) (delete-directory dir t)))))

(ert-deftest tilde-editing/smerge-enables-for-conflict-markers ()
  (tilde-editing--load)
  (with-temp-buffer
    (insert "<<<<<<< ours\na\n=======\nb\n>>>>>>> theirs\n")
    (emacs-lisp-mode)
    (my/maybe-enable-smerge-mode)
    (should smerge-mode)))
