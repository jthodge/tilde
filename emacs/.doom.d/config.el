;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-oceanic-next)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)
;; or for treemacs users
(setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
(doom-themes-treemacs-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Taylor Hodge"
      user-mail-address "j.taylor.hodge@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq projectile-project-search-path '("~/01-projects/" "~/02-areas/" "~/03-resources/" "~/04-archives/" "~/tilde/" "~/z/"))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-compact-blocks t
      org-agenda-start-day nil ;; i.e. today
      org-agenda-span 1
      org-agenda-start-on-weekday nil)
  (setq org-super-agenda-groups '((:name "Today"
                                   :time-grid t
                                   :scheduled today)
                                  (:name "Due today"
                                   :deadline today)
                                  ;; (:name "Important"
                                  ;;  :priority "A")
                                  (:name "Late"
                                   :deadline past)
                                  (:name "Upcoming"
                                   :deadline future)))
                                  ;; (:name "Large impact"
                                  ;;  :tag "li")))
 :config
  (org-super-agenda-mode))

(defun make-youtube-link (youtube_id)
  (browse-url (concat "https://www.youtube.com/embed/" youtube_id)))

(after! org
  (org-add-link-type "yt" #'make-youtube-link)
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup)
  (setq
   org-agenda-files '("~/org/inbox.org"
                      "~/org/tasks.org"
                      "~/org/01-projects.org"
                      "~/org/02-areas.org"
                      "~/org/03-resources.org"
                      "~/org/04-archives.org"
                      "~/org/tickler.org")
   org-capture-templates '(("t" "Task" entry (file+headline "~/org/todo.org" "Inbox")
                            "* TODO %?\n")
                           ("p" "Project" entry (file+headline "~/org/todo.org" "01 Projects")
                            (file "~/org/templates/new-project-template.org"))
                           ("s" "Someday" entry (file+headline "~/org/someday-maybe.org" "Someday || Maybe")
                            "* SOMEDAY %?\n")
                           ("m" "Maybe" entry (file+headline "~/org/someday-maybe.org" "Someday || Maybe")
                            "* MAYBE %?\n")
                           ("j" "Journal" entry (file+datetree "~/org/journal.org")
                            (file "~/org/templates/new-journal-template.org")))
   org-log-done t
   org-refile-targets '((org-agenda-files :maxlevel . 3))
   org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" "BLOCKED(b)" "|" "DONE(d)" "CANCELED(C)"))
   org-todo-keyword-faces '(("TODO" :foreground "#7c7c75" :weight normal :underline t)
                            ("INPROGRESS" :foreground "#0098dd" :weight normal :underline t)
                            ("BLOCKED" :foreground "#9f7efe" :weight normal :underline t)
                            ("DONE" :foreground "#50a14f" :weight normal :underline t)
                            ("CANCELED" :foreground "#ff6480" :weight normal :underline t))))

(map! :desc "Create Sparse Tree" :ne "SPC / s" #'org-sparse-tree)
(map! :desc "Create Sparse Tree for Tags" :ne "SPC / t" #'org-tags-sparse-tree)

