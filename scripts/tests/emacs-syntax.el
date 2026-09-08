;;; emacs-syntax.el --- Read tracked Lisp without evaluating it -*- lexical-binding: t; -*-
(let* ((root (expand-file-name "../.." (file-name-directory load-file-name)))
       (default-directory root)
       (paths (with-temp-buffer
                (unless (zerop (process-file "git" nil t nil "ls-files" "-z"))
                  (error "Cannot enumerate tracked files"))
                (split-string (buffer-string) "\0" t)))
       (count 0))
  (dolist (path paths)
    (when (string-suffix-p ".el" path)
      (with-temp-buffer
        (insert-file-contents (expand-file-name path root))
        (emacs-lisp-mode)
        (condition-case error
            (check-parens)
          (error (error "%s: %s" path error))))
      (setq count (1+ count))))
  (princ (format "PASS: %d tracked Lisp files pass check-parens\n" count)))
