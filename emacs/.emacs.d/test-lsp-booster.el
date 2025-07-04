;; LSP Booster and Plist Testing Functions

(defun test-lsp-configuration ()
  "Test current LSP configuration and display results."
  (interactive)
  (with-current-buffer (get-buffer-create "*LSP Configuration Test*")
    (erase-buffer)
    (insert "=== LSP Configuration Test Results ===\n\n")

    ;; Check plist configuration
    (insert (format "1. Plist Configuration:\n"))
    (insert (format "   - lsp-use-plists: %s\n" (bound-and-true-p lsp-use-plists)))
    (insert (format "   - LSP_USE_PLISTS env: %s\n" (getenv "LSP_USE_PLISTS")))

    ;; Check LSP Booster
    (insert (format "\n2. LSP Booster:\n"))
    (insert (format "   - Executable found: %s\n"
                    (or (my/lsp-booster-find-executable) "NOT FOUND")))

    ;; Check if advice is active
    (insert (format "\n3. Advice Functions:\n"))
    (insert (format "   - json-parse advice: %s\n"
                    (advice-member-p #'lsp-booster--advice-json-parse
                                     (if (fboundp 'json-parse-buffer)
                                         'json-parse-buffer
                                       'json-read))))
    (insert (format "   - command advice: %s\n"
                    (advice-member-p #'lsp-booster--advice-final-command
                                     'lsp-resolve-final-command)))

    ;; Check data structures
    (insert (format "\n4. Data Structure Type:\n"))
    (when (and (boundp 'lsp--cur-workspace) lsp--cur-workspace)
      (let* ((workspace lsp--cur-workspace)
             (client (lsp--workspace-client workspace))
             (server-id (lsp--client-server-id client)))
        (insert (format "   - Server: %s\n" server-id))
        (insert (format "   - Data type: %s\n"
                        (if lsp-use-plists "PLISTS" "HASH-TABLES")))))

    ;; Performance metrics
    (insert (format "\n5. Performance Settings:\n"))
    (insert (format "   - read-process-output-max: %s bytes\n" read-process-output-max))
    (insert (format "   - gc-cons-threshold: %s bytes\n" gc-cons-threshold))
    (insert (format "   - gc-cons-percentage: %s\n" gc-cons-percentage))

    (display-buffer (current-buffer))
    (goto-char (point-min))))

(defun benchmark-lsp-completion ()
  "Benchmark LSP completion performance."
  (interactive)
  (if (not (bound-and-true-p lsp-mode))
      (message "LSP mode is not active in this buffer")
    (let ((start-time (current-time)))
      ;; Trigger completion
      (completion-at-point)
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        (message "Completion took %.3f seconds" elapsed)))))

(defun check-lsp-server-process ()
  "Check if LSP server is running through emacs-lsp-booster."
  (interactive)
  (if-let ((workspace (lsp-workspaces)))
      (let* ((proc (lsp--workspace-proc (car workspace)))
             (cmd (process-command proc)))
        (message "LSP Server command: %s" cmd)
        (if (member "emacs-lsp-booster" cmd)
            (message "✓ LSP Booster is ACTIVE")
          (message "✗ LSP Booster is NOT active")))
    (message "No LSP workspace found")))

(defun inspect-lsp-messages ()
  "Enable LSP I/O logging temporarily to inspect message format."
  (interactive)
  (setq lsp-log-io t)
  (message "LSP I/O logging enabled. Check *lsp-log* buffer for messages.")
  (run-with-timer 30 nil (lambda ()
                          (setq lsp-log-io nil)
                          (message "LSP I/O logging disabled."))))

;; Keybindings for testing
(global-set-key (kbd "C-c t l") 'test-lsp-configuration)
(global-set-key (kbd "C-c t b") 'benchmark-lsp-completion)
(global-set-key (kbd "C-c t p") 'check-lsp-server-process)
(global-set-key (kbd "C-c t i") 'inspect-lsp-messages)
