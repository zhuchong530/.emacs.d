;;; Package -- Summary
;;; Commentary:
;;; Code:

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :custom
  (setq org-log-done t)
  (setq org-agenda-files (list "~/Google Driver/All Notes/Agenda/work.org"
                               "~/Google Driver/All Notes/Agenda/study.org"
                               "~/Google Driver/All Notes/Agenda/life.org"))
  (setq org-todo-keywords
        '((sequence "TODO" "IN_PROGRESS" "WAITING" "DONE")))
  (setq org-tag-alist '(("@work" . ?w) ("@study" . ?s)))
  )


(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq org-bullets-bullet-list '("☰" "☷" "☯" "☭"))
(setq org-ellipsis " ▼ ")
(setq org-src-fontify-natively t)


(provide 'setup-org)
;;; setup-org.el ends here
