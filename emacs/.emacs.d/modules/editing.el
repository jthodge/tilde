;;; editing.el --- Small editing conveniences -*- lexical-binding: t; -*-

(defun my/fill-or-unfill ()
  "Fill paragraph normally, then unfill it when repeated.

The second consecutive invocation temporarily sets `fill-column' to
`point-max', keeping the command under our own namespace. Remapping
`fill-paragraph' means the normal key continues to work."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'my/fill-or-unfill)
             (progn
               ;; Make a third invocation fill again instead of staying in
               ;; unfill mode forever.
               (setq this-command nil)
               (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph] #'my/fill-or-unfill)

(defun my/delete-current-buffer-file ()
  "Delete the current buffer's file after confirmation, then kill the buffer.

If the buffer is not visiting an existing file, only offer to kill the buffer.
This is intentionally interactive-only: no hook calls it, and it never deletes
without an explicit `yes-or-no-p' confirmation."
  (interactive)
  (let ((file (buffer-file-name)))
    (cond
     ((not file)
      (when (yes-or-no-p "Buffer has no file; kill it? ")
        (kill-buffer)))
     ((not (file-exists-p file))
      (when (yes-or-no-p "Visited file is missing; kill buffer? ")
        (kill-buffer)))
     ((yes-or-no-p (format "Delete %s and kill its buffer? " file))
      (delete-file file)
      (kill-buffer)
      (message "Deleted %s" file)))))

(defun my/maybe-enable-smerge-mode ()
  "Enable `smerge-mode' when the file contains conflict markers."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook #'my/maybe-enable-smerge-mode t)

(provide 'editing)
