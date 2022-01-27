;;; Package -- Summary
;;; Commentary:
;;; Code:

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-use-speed-commands t
        org-return-follows-link t
        org-hide-emphasis-markers t     ; don't display the emphasis markers
        org-outline-path-complete-in-steps nil
        org-startup-indented t
        org-startup-folded 'content
        org-fontify-done-headline t     ;change the face of a headline if it's marked DONE
        org-src-fontity-native t        ;Pretty code blocks
        org-pretty-entities t           ;show entities as UTF-8 characters
        org-hide-leading-stars t        ;hide the stars
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil)
        (setq org-todo-keywords
              '(("⚑ TODO(t)" "|" "✔ DONE(d)" "⚐ WAITING(w)" "|" "✘ CANCELED(c)")))
        (add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode)) ;Journal entries
        :custom
        (setq org-log-done 'time)
        (setq org-agenda-files (list "~/Google Driver/All Notes/Agenda/work.org"
                                     "~/Google Driver/All Notes/Agenda/study.org"
                                     "~/Google Driver/All Notes/Agenda/life.org"))
        :config
        (font-lock-add-keywords            ; A bit silly but my headers are now
         'org-mode `(("^\\*+ \\(TODO\\) "  ; shorter, and that is nice canceled
                      (1 (progn (compose-region (match-beginning 1) (match-end 1) "⚑")
                                nil)))
                     ("^\\*+ \\(DOING\\) "
                      (1 (progn (compose-region (match-beginning 1) (match-end 1) "⚐")
                                nil)))
                     ("^\\*+ \\(CANCELED\\) "
                      (1 (progn (compose-region (match-beginning 1) (match-end 1) "✘")
                                nil)))
                     ("^\\*+ \\(DONE\\) "
                      (1 (progn (compose-region (match-beginning 1) (match-end 1) "✔")
                                nil)))))
        (define-key org-mode-map (kbd "M-C-n") 'org-end-of-item-list)
        (define-key org-mode-map (kbd "M-C-p") 'org-beginning-of-item)
        (define-key org-mode-map (kbd "M-C-u") 'outline-up-heading)
        (define-key org-mode-map (kbd "M-C-w") 'org-table-copy-region)
        (define-key org-mode-map (kbd "M-C-y") 'org-table-paste-rectangle)
        (global-set-key (kbd "C-c l") #'org-store-link)
        (global-set-key (kbd "C-c a") #'org-agenda)
        (global-set-key (kbd "C-c c") #'org-capture)
        (add-hook 'org-mode-hook
                  (lambda ()
                    (variable-pitch-mode 1)
                    visual-line-mode))
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
  (setq org-bullets-bullet-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))
  (setq org-ellipsis "▼"))

;; for '<s/e/c... TAB'  completion
;; or "C-c C-,"
(require 'org-tempo)

(use-package ob-go)
(use-package ox-reveal)
(use-package ox-gfm)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   (C . t)
   ;; (CPP . t)
   ;; (dot . t)
   ;; (plantuml . t)
   (go . t)))

;; maintain TOC(table of contents) automatically
;; put :TOC: tag to a heading, run M-x toc-org-insert-toc
(use-package toc-org
  :after org)

;; For HTML exports
(use-package org-drill
  :ensure org-plus-contrib)
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




(provide 'setup-org)
;;; setup-org.el ends here
