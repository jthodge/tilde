;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; KEYBINDINGS
;;; ================================================================

;; Org mode keybindings (prevent conflicts with lsp-mode)
(global-set-key (kbd "C-c o l") #'org-store-link)
(global-set-key (kbd "C-c o b") #'org-switchb)
