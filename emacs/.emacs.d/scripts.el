;;; scripts.el --- Hand-rolled toolbox -*- lexical-binding: t; -*-

(defun ascii-to-hex (beg end)
  "Convert region (or whole buffer) from ASCII to contiguous *uppercase* hex.
If the region is active use BEG END. Otherwise, operate on the entire buffer.

E.g.:
  ABC -> 414243
  \"\\n\" -> 0A"
  (interactive "r")
  ;; widen to full buffer when no region is active
  (unless (use-region-p)
    (setq beg (point-min) end (point-max)))
  (let ((hex (mapconcat
              (lambda (char)
                (format "%02X" char))        ; \"%02X\" => two-digit uppercase
              (string-to-list
               (buffer-substring-no-properties beg end))
              "")))
    (delete-region beg end)
    (insert hex)))
(provide 'ascii-to-hex)

;; TODO: fix bugs
(defun escape-text (beg end)
  "Escape backslash, single-quote, double-quote and NUL in region or buffer.
If a region is active, operate on it. Otherwise process the whole buffer.
Result matches PHP’s `addslashes`."
  (interactive "r")
  ;; Widen to buffer if no active region
  (unless (use-region-p)
    (setq beg (point-min)
          end (point-max)))
  (let* ((text (buffer-substring-no-properties beg end))
         ;; First escape \  '  "
         (step1 (replace-regexp-in-string
                 "[\\\\\"']" "\\\\\\&" text t t))
         ;; Then replace NUL (char code 0) with \0
         (escaped (replace-regexp-in-string
                   (string 0) "\\\\0" step1 t t)))
    (delete-region beg end)
    (insert escaped)))
(provide 'escape-texit)

;;; ================================================================
;;; COMBOBULATE TESTING UTILITIES
;;; ================================================================

(defun my/test-combobulate-setup ()
  "Test if Combobulate is properly installed and configured."
  (interactive)
  (let ((results '()))
    ;; Check package installation
    (if (package-installed-p 'combobulate)
        (push "✅ Combobulate package is installed" results)
      (push "❌ Combobulate package is NOT installed" results))
    
    ;; Check if combobulate feature is available
    (if (featurep 'combobulate)
        (push "✅ Combobulate feature is loaded" results)
      (push "⚠️  Combobulate feature is not yet loaded (normal until first use)" results))
    
    ;; Check key bindings
    (if (key-binding (kbd "C-c o n"))
        (push "✅ Combobulate key bindings are configured" results)
      (push "❌ Combobulate key bindings are NOT configured" results))
    
    ;; Display results
    (with-output-to-temp-buffer "*Combobulate Test Results*"
      (dolist (result (reverse results))
        (princ result)
        (princ "\n"))
      (princ "\n")
      (princ "Available Combobulate commands with C-c o prefix:\n")
      (princ "- C-c o n/p: Navigate next/previous\n")
      (princ "- C-c o u/d: Navigate up/down\n")
      (princ "- C-c o m/M: Mark node (expand/contract)\n")
      (princ "- C-c o t: Transpose siblings\n")
      (princ "- C-c o k: Kill node\n")
      (princ "- C-c o c: Clone node\n")
      (princ "- C-c o h: Highlight node\n")
      (princ "- C-c o e: Edit node\n"))))

(defun my/test-structural-navigation ()
  "Test Combobulate structural navigation in current buffer."
  (interactive)
  (let ((results '())
        (mode-name (symbol-name major-mode)))
    ;; Check if we're in a supported mode
    (if (or (eq major-mode 'typescript-ts-mode)
            (eq major-mode 'tsx-ts-mode)
            (eq major-mode 'javascript-ts-mode)
            (eq major-mode 'js-ts-mode))
        (push (format "✅ Buffer is in supported mode: %s" mode-name) results)
      (push (format "⚠️  Buffer is in mode: %s (may not be fully supported)" mode-name) results))
    
    ;; Check if combobulate-mode is active
    (if (and (boundp 'combobulate-mode) combobulate-mode)
        (push "✅ Combobulate mode is active in this buffer" results)
      (push "❌ Combobulate mode is NOT active in this buffer" results))
    
    ;; Check if tree-sitter is working
    (if (and (fboundp 'treesit-node-at) (treesit-node-at (point)))
        (push "✅ Tree-sitter is working (node detected at point)" results)
      (push "❌ Tree-sitter is not working or no node at point" results))
    
    ;; Display results
    (with-output-to-temp-buffer "*Structural Navigation Test*"
      (dolist (result (reverse results))
        (princ result)
        (princ "\n"))
      (princ "\n")
      (princ "To test navigation:\n")
      (princ "1. Place cursor on a function, variable, or expression\n")
      (princ "2. Try C-c o n (next) and C-c o p (previous)\n")
      (princ "3. Try C-c o u (up) and C-c o d (down)\n")
      (princ "4. Try C-c o m to mark/expand selection\n"))))

(defun my/test-tree-sitter-modes ()
  "Test if tree-sitter modes are properly configured."
  (interactive)
  (let ((results '()))
    ;; Check if tree-sitter is available
    (if (and (fboundp 'treesit-available-p) (treesit-available-p))
        (push "✅ Tree-sitter is available in this Emacs build" results)
      (push "❌ Tree-sitter is NOT available in this Emacs build" results))
    
    ;; Check language grammars
    (let ((required-grammars '(typescript tsx javascript json css html yaml)))
      (dolist (grammar required-grammars)
        (if (treesit-language-available-p grammar)
            (push (format "✅ %s grammar is installed" grammar) results)
          (push (format "❌ %s grammar is NOT installed" grammar) results))))
    
    ;; Check mode remappings
    (let ((mode-remaps '((typescript-mode . typescript-ts-mode)
                         (js-mode . typescript-ts-mode)
                         (json-mode . json-ts-mode))))
      (dolist (remap mode-remaps)
        (if (assq (car remap) major-mode-remap-alist)
            (push (format "✅ %s → %s remapping configured" (car remap) (cdr remap)) results)
          (push (format "❌ %s → %s remapping NOT configured" (car remap) (cdr remap)) results))))
    
    ;; Display results  
    (with-output-to-temp-buffer "*Tree-sitter Test Results*"
      (dolist (result (reverse results))
        (princ result)
        (princ "\n"))
      (princ "\n")
      (princ "File associations:\n")
      (princ "- .ts files → typescript-ts-mode\n")
      (princ "- .tsx files → tsx-ts-mode\n")
      (princ "- .js files → typescript-ts-mode\n")
      (princ "- .jsx files → tsx-ts-mode\n")
      (princ "- .json files → json-ts-mode\n"))))

(provide 'combobulate-testing)
