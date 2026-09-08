;;; -*- lexical-binding: t; -*-
;;; ================================================================
;;; CUSTOM SETTINGS
;;; ================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f747c4004e38bcdc131649a90325c00d246bb7dc73bc6ab6e0e7ab5489da8459"
     "a4340c197a450c77c729cad236b5f3ca88aaf974e91a7af2d2e7ae7bb5f96720"
     "6b20d669fcbcd79c6d0f3db36a71af1b88763246d3550a0c361866adecb38a9e"
     default))
 '(package-selected-packages nil)
 '(safe-local-variable-values
   '((lsp-typescript-suggest-auto-imports . t)
     (lsp-typescript-format-enable)
     (lsp-typescript-preferences-quote-style . "single")
     (eval when (executable-find "node")
           (setenv "NODE_PATH"
                   (expand-file-name "node_modules" default-directory)))
     (eval setq-local lsp-server-install-dir
           (expand-file-name "node_modules" default-directory)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
