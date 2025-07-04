;; Apheleia Testing Functions

(defun test-apheleia-configuration ()
  "Test Apheleia configuration and display results."
  (interactive)
  (with-current-buffer (get-buffer-create "*Apheleia Test Results*")
    (erase-buffer)
    (insert "=== Apheleia Configuration Test Results ===\n\n")
    
    ;; Check if Apheleia is available and configured
    (insert (format "1. Apheleia Package Status:\n"))
    (insert (format "   - Package installed: %s\n" (package-installed-p 'apheleia)))
    (insert (format "   - Feature loaded: %s\n" (featurep 'apheleia)))
    (insert (format "   - Global mode active: %s\n" (bound-and-true-p apheleia-global-mode)))
    
    ;; Check formatter availability
    (insert (format "\n2. Volta Formatter Status:\n"))
    (insert (format "   - Prettier path: %s\n" 
                    (if (file-executable-p "/Users/jth/.volta/bin/prettier") 
                        "/Users/jth/.volta/bin/prettier" 
                        "NOT FOUND")))
    (insert (format "   - ESLint path: %s\n" 
                    (if (file-executable-p "/Users/jth/.volta/bin/eslint") 
                        "/Users/jth/.volta/bin/eslint" 
                        "NOT FOUND")))
    
    ;; Check mode associations
    (insert (format "\n3. Mode Associations:\n"))
    (when (boundp 'apheleia-mode-alist)
      (insert (format "   - typescript-ts-mode: %s\n" (alist-get 'typescript-ts-mode apheleia-mode-alist)))
      (insert (format "   - tsx-ts-mode: %s\n" (alist-get 'tsx-ts-mode apheleia-mode-alist)))
      (insert (format "   - json-ts-mode: %s\n" (alist-get 'json-ts-mode apheleia-mode-alist))))
    
    ;; Check current buffer
    (insert (format "\n4. Current Buffer:\n"))
    (insert (format "   - Major mode: %s\n" major-mode))
    (insert (format "   - Apheleia mode active: %s\n" (bound-and-true-p apheleia-mode)))
    (insert (format "   - Associated formatter: %s\n" 
                    (when (boundp 'apheleia-mode-alist)
                      (alist-get major-mode apheleia-mode-alist))))
    
    ;; Instructions
    (insert (format "\n5. Usage Instructions:\n"))
    (insert (format "   - Format buffer manually: C-c f\n"))
    (insert (format "   - Auto-format on save: Enabled globally\n"))
    (insert (format "   - Toggle apheleia: M-x apheleia-mode\n"))
    
    (display-buffer (current-buffer))
    (goto-char (point-min))))

(defun test-apheleia-formatting ()
  "Test Apheleia formatting on current buffer."
  (interactive)
  (if (not (bound-and-true-p apheleia-mode))
      (message "Apheleia mode is not active in this buffer")
    (let ((start-time (current-time)))
      (apheleia-format-buffer)
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        (message "Apheleia formatting took %.3f seconds" elapsed)))))

(defun create-test-typescript-file ()
  "Create a test TypeScript file with intentionally bad formatting."
  (interactive)
  (let ((test-content "const foo={bar:123,baz:'hello world'};
function test(  a: string,b:number  ) {
return{result:a+b.toString(),status:'ok'};}
const arr=[1,2,3,4,5].map(x=>x*2).filter(x=>x>5);"))
    (with-current-buffer (get-buffer-create "*test-formatting.ts*")
      (typescript-ts-mode)
      (erase-buffer)
      (insert test-content)
      (goto-char (point-min))
      (display-buffer (current-buffer))
      (message "Test TypeScript file created. Try C-c f to format or save to auto-format."))))

;; Key bindings for testing
(global-set-key (kbd "C-c t a") 'test-apheleia-configuration)
(global-set-key (kbd "C-c t F") 'test-apheleia-formatting)
(global-set-key (kbd "C-c t T") 'create-test-typescript-file)