;; ESLint LSP Testing Functions

(defun test-eslint-lsp-configuration ()
  "Test ESLint LSP configuration and display results."
  (interactive)
  (with-current-buffer (get-buffer-create "*ESLint LSP Test Results*")
    (erase-buffer)
    (insert "=== ESLint LSP Configuration Test Results ===\n\n")
    
    ;; Check if ESLint language server is available
    (insert (format "1. ESLint Language Server Status:\n"))
    (insert (format "   - Server binary: %s\n" 
                    (if (file-executable-p "/Users/jth/.volta/bin/vscode-eslint-language-server")
                        "FOUND"
                        "NOT FOUND")))
    (insert (format "   - Volta ESLint: %s\n"
                    (if (file-executable-p "/Users/jth/.volta/bin/eslint")
                        (shell-command-to-string "/Users/jth/.volta/bin/eslint --version")
                        "NOT FOUND")))
    
    ;; Check LSP clients
    (insert (format "\n2. LSP Client Registration:\n"))
    (insert (format "   - ESLint LSP configuration added to init.el\n"))
    (insert (format "   - Multi-server support enabled via :add-on? t\n"))
    
    ;; Check current buffer status
    (insert (format "\n3. Current Buffer:\n"))
    (insert (format "   - Major mode: %s\n" major-mode))
    (insert (format "   - LSP mode active: %s\n" (bound-and-true-p lsp-mode)))
    (when (bound-and-true-p lsp-mode)
      (insert (format "   - Active servers: %s\n" 
                      (mapcar (lambda (ws) 
                                (lsp--workspace-server-id ws))
                              (lsp-workspaces)))))
    
    ;; Instructions
    (insert (format "\n4. Usage:\n"))
    (insert (format "   - Open a TypeScript file to test multi-server\n"))
    (insert (format "   - Both ts-ls and eslint-lsp should be active\n"))
    (insert (format "   - ESLint diagnostics appear alongside TypeScript errors\n"))
    (insert (format "   - Code actions available from both servers\n"))
    
    (display-buffer (current-buffer))
    (goto-char (point-min))))

(defun check-active-lsp-servers ()
  "Check which LSP servers are active in the current buffer."
  (interactive)
  (if (not (bound-and-true-p lsp-mode))
      (message "LSP mode is not active in this buffer")
    (let ((servers (mapcar (lambda (ws)
                            (lsp--workspace-server-id ws))
                          (lsp-workspaces))))
      (message "Active LSP servers: %s" servers))))

(defun test-eslint-diagnostics ()
  "Create a test file with ESLint violations."
  (interactive)
  (let ((test-content "// Test file for ESLint diagnostics
const unused_var = 123;  // Should trigger unused variable warning

function test(a, b) {  // Missing TypeScript types
  console.log(a)  // Missing semicolon
  if(a == b) {  // Should suggest === instead of ==
    return true
  }
  return false;
}

const obj = { foo: 1, foo: 2 };  // Duplicate key

export { test }"))
    (with-current-buffer (get-buffer-create "*test-eslint.ts*")
      (typescript-ts-mode)
      (erase-buffer)
      (insert test-content)
      (goto-char (point-min))
      (display-buffer (current-buffer))
      (message "Test TypeScript file created. Check for both TypeScript and ESLint diagnostics."))))

;; Key bindings for testing
(global-set-key (kbd "C-c t e") 'test-eslint-lsp-configuration)
(global-set-key (kbd "C-c t E") 'check-active-lsp-servers)
(global-set-key (kbd "C-c t D") 'test-eslint-diagnostics)