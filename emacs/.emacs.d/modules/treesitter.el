;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; TREE-SITTER CONFIGURATION
;;; ================================================================

;;; ----------------------------------------------------------------
;;; Core Tree-sitter Setup
;;; ----------------------------------------------------------------

(defun my/setup-treesitter-grammars ()
  "Install Tree-sitter grammars if they're absent."
  (interactive)
  (when (and (fboundp 'treesit-install-language-grammar)
             (boundp 'treesit-language-source-alist))
    (let ((grammars '(;; TODO: C language support
                      ;; (c "https://github.com/tree-sitter/tree-sitter-c")
                      ;; TODO: CMake language support
                      ;; (cmake "https://github.com/uyha/tree-sitter-cmake")
                      ;; TODO: C++ language support
                      ;; (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                      (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                      (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                      ;; TODO: Go language support
                      ;; (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
                      (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                      (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
                      (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                      (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                      ;; TODO: Make language support
                      ;; (make "https://github.com/alemuller/tree-sitter-make")
                      ;; TODO: Markdown language support
                      ;; (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                      ;; TODO: TOML language support
                      ;; (toml "https://github.com/tree-sitter/tree-sitter-toml")
                      (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                      (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                      ;; TODO: YAML language support
                      ;; (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
                      )))
      (dolist (grammar grammars)
        (add-to-list 'treesit-language-source-alist grammar)
        (condition-case err
            (unless (treesit-language-available-p (car grammar))
              (treesit-install-language-grammar (car grammar)))
          (error
           (message "Failed to install tree-sitter grammar for %s: %s" (car grammar) err)))))))

;;; ----------------------------------------------------------------
;;; Mode Remapping
;;; ----------------------------------------------------------------

(defun my/setup-treesitter-mode-remapping ()
  "Configure major mode remapping for tree-sitter modes."
  (when (and (fboundp 'treesit-available-p)
             (boundp 'major-mode-remap-alist))
    (let ((mode-mappings '(;; TODO: (go-mode . go-ts-mode)
                           (typescript-mode . typescript-ts-mode)
                           (js-mode . js-ts-mode)
                           (js2-mode . js-ts-mode)
                           (json-mode . json-ts-mode)
                           (js-json-mode . json-ts-mode))))
      (dolist (mapping mode-mappings)
        (add-to-list 'major-mode-remap-alist mapping)))))

;;; ----------------------------------------------------------------
;;; File Associations
;;; ----------------------------------------------------------------

(defun my/setup-treesitter-auto-modes ()
  "Configure file associations for tree-sitter modes."
  (when (fboundp 'treesit-available-p)
    (let ((file-associations '(;; TODO: ("\\.go\\'" . go-ts-mode)
                               ("\\.tsx\\'" . tsx-ts-mode)
                               ("\\.ts\\'" . typescript-ts-mode)
                               ("\\.jsx\\'" . tsx-ts-mode)
                               ("\\.js\\'" . js-ts-mode)
                               ("\\.mjs\\'" . js-ts-mode)
                               ("\\.mts\\'" . typescript-ts-mode)
                               ("\\.cjs\\'" . js-ts-mode)
                               ("\\.json\\'" . json-ts-mode))))
      (dolist (association file-associations)
        (add-to-list 'auto-mode-alist association)))))

;;; ----------------------------------------------------------------
;;; Initialize Tree-sitter
;;; ----------------------------------------------------------------

(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (my/setup-treesitter-grammars)
  (my/setup-treesitter-mode-remapping)
  (my/setup-treesitter-auto-modes))
