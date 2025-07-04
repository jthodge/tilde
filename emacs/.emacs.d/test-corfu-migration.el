;; Corfu Migration Testing Functions

(defun test-corfu-configuration ()
  "Test Corfu configuration and display results."
  (interactive)
  (with-current-buffer (get-buffer-create "*Corfu Migration Test*")
    (erase-buffer)
    (insert "=== Corfu Migration Test Results ===\n\n")
    
    ;; Check if Corfu is available and configured
    (insert (format "1. Corfu Package Status:\n"))
    (insert (format "   - Package installed: %s\n" (package-installed-p 'corfu)))
    (insert (format "   - Feature loaded: %s\n" (featurep 'corfu)))
    (insert (format "   - Global mode active: %s\n" (bound-and-true-p global-corfu-mode)))
    
    ;; Check Cape
    (insert (format "\n2. Cape Package Status:\n"))
    (insert (format "   - Package installed: %s\n" (package-installed-p 'cape)))
    (insert (format "   - Feature loaded: %s\n" (featurep 'cape)))
    
    ;; Check completion-at-point-functions
    (insert (format "\n3. Completion Functions:\n"))
    (insert (format "   - completion-at-point-functions: %s\n" completion-at-point-functions))
    
    ;; Check Company status
    (insert (format "\n4. Company Status (should be disabled):\n"))
    (insert (format "   - Company mode active: %s\n" (bound-and-true-p company-mode)))
    (insert (format "   - Global company mode: %s\n" (bound-and-true-p global-company-mode)))
    
    ;; Check LSP completion provider
    (insert (format "\n5. LSP Integration:\n"))
    (insert (format "   - lsp-completion-provider: %s\n" 
                    (if (boundp 'lsp-completion-provider) 
                        lsp-completion-provider 
                        "not set")))
    
    ;; Current buffer completion setup
    (insert (format "\n6. Current Buffer:\n"))
    (insert (format "   - Major mode: %s\n" major-mode))
    (insert (format "   - LSP mode active: %s\n" (bound-and-true-p lsp-mode)))
    (insert (format "   - Yasnippet active: %s\n" (bound-and-true-p yas-minor-mode)))
    
    (display-buffer (current-buffer))
    (goto-char (point-min))))

(defun test-completion-performance ()
  "Test completion performance with Corfu."
  (interactive)
  (if (not (bound-and-true-p lsp-mode))
      (message "LSP mode is not active in this buffer")
    (let ((start-time (current-time)))
      ;; Trigger completion
      (completion-at-point)
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        (message "Corfu completion took %.3f seconds" elapsed)))))

(defun toggle-company-corfu ()
  "Toggle between Company and Corfu for testing."
  (interactive)
  (if (bound-and-true-p global-corfu-mode)
      (progn
        (global-corfu-mode -1)
        (global-company-mode 1)
        (message "Switched to Company mode"))
    (progn
      (global-company-mode -1)
      (global-corfu-mode 1)
      (message "Switched to Corfu mode"))))

;; Keybindings for testing
(global-set-key (kbd "C-c t c") 'test-corfu-configuration)
(global-set-key (kbd "C-c t f") 'test-completion-performance)
(global-set-key (kbd "C-c t s") 'toggle-company-corfu)