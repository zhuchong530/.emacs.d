;;; Package -- Summary
;;; Commentary:
;;; Code:

(use-package org
  :pin gnu
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . visual-line-mode)
  :init (setq
         org-use-speed-commands t
         org-return-follows-link t
         org-hide-emphasis-markers t     ; don't display the emphasis markers
         org-outline-path-complete-in-steps nil
         org-startup-indented t
         org-startup-folded 'content
         org-fontify-done-headline t     ;change the face of a headline if it's marked DONE
         org-src-fontity-native t        ;Pretty code blocks
         org-pretty-entities t           ;show entities as UTF-8 characters
         org-hide-leading-stars t        ;hide the stars
         org-src-tab-acts-natively t     ;Make TAB acts as if it were issued from the buffer of the languages's major mode
         truncate-lines nil
         org-confirm-babel-evaluate nil) ;don't notify -> "Do you want to execute"
  :config (setq
           org-directory (file-truename "~/Google Driver/All Notes")
           org-todo-keywords
           '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((emacs-lisp . t)
                                         (python . t)
                                         (shell . t)
                                         (C . t)
                                         (js . t)
                                         (go . t))))
  :custom
  (setq org-log-done 'time)
  (setq org-agenda-files (list "~/Google Driver/All Notes/Agenda/work.org"
                               "~/Google Driver/All Notes/Agenda/study.org"
                               "~/Google Driver/All Notes/Agenda/life.org"))
  :bind(("C-c l" . org-store-link)
        ("C-c a" . org-agenda)
        ("C-c c" . org-capture))
  )

;; really need this package to set the org-bullets by yourself
(use-package org-bullets
  :init (add-hook 'org-mode-hook 'org-bullets-mode)
  :config
  ;; (setq org-bullets-bullet-list '("☰" "☷" "☯" "☭" "✸" "✿"))
  ;; hexagrams
  ;; (setq org-bullets-bullet-list '("✡" "⎈" "✽" "✲" "✱" "✻" "✼" "✽" "✾" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇"))
  ;; circles
  ;; (setq org-bullets-bullet-list '("○" "☉" "◎" "◉" "○" "◌" "◎" "●" "◦" "◯" "⚪" "⚫" "⚬" "❍" "￮" "⊙" "⊚" "⊛" "∙" "∘"))
  ;; special circles
  ;; (setq org-bullets-bullet-list '("◐" "◑" "◒" "◓" "◴" "◵" "◶" "◷" "⚆" "⚇" "⚈" "⚉" "♁" "⊖" "⊗" "⊘"))
  ;; crosses
  ;; (setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))
  ;; poker sybmols
  ;; (setq org-bullets-bullet-list '("♠" "♣" "♥" "♦" "♤" "♧" "♡" "♢"))
  ;; special symbols
  ;; (setq org-bullets-bullet-list '("☀" "♼" "☼" "☾" "☽" "☣" "§" "¶" "‡" "※" "✕" "△" "◇" "▶" "◀" "◈"))
  (setq org-bullets-bullet-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷")) ;
  (setq org-ellipsis "▼"))

;; for '<s/e/c... TAB'  completion
;; or "C-c C-,"
(require 'org-tempo)

(use-package ob-go)
(use-package ox-reveal)
(use-package ox-gfm)



;; maintain TOC(table of contents) automatically
;; put :TOC: tag to a heading, run M-x toc-org-insert-toc
(use-package toc-org
  :after org)


;; WYSIWYG, html mime composition using org-mode
(use-package org-mime)

;; TODO
(use-package org-download)

;; Journaling
(use-package org-journal
  :init
  (setq org-journal-dir "~/Google Driver/All Notes/journal/")
  (setq org-journal-date-format "#+TITLE: Journal Entry- %e %b %Y (%A)")
  (setq org-journal-time-format ""))

;; A function to easily load today's journal entry
(defun get-journal-file-today ()
  "Return filename for today's journal entry."
  (let ((dialy-name (format-time-string "%Y%m%d")))
    (expand-file-name (concat org-journal-dir daily-name))))
(defun journal-file-today ()
  "Create and load a journal file based on today's date"
  (interactive)
  (find-file (get-journal-file-today)))
(global-set-key (kbd "C-c f j") 'journal-file-today)


(defun get-journal-file-yesterday ()
  "Return filename for yesterday's journal entry."
  (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
         (daily-name (format-time-string "%Y%m%d" yesterday)))
    (expand-file-name (concat org-journal-dir dialy-name))))
(defun journal-file-yesterday ()
  "Creates and load a file based on yesterday's date."
  (interactive)
  (find-file (get-journal-file-yesterday)))
(global-set-key (kbd "C-c f y") 'journal-file-yesterday)


;; org-roam
;; org-roam is Version 2 now, So we use org-roam-ui for graphic,
;; not org-roam-server which only support org-roam Version 1
(use-package org-roam
  :custom
  (org-roam-directory "~/Google Driver/All Notes/")
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-completion-everywhere t)
  (add-hook 'after-init-hook 'org-roam-mode)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config (org-roam-setup)
  )

;; org-roam-ui
(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))




(provide 'setup-org)
;;; setup-org.el ends here
