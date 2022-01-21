;;; Package -- Summary
;;; Commentary:
;;; Code:

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :custom
  (setq org-bullets-bullet-list '("☰" "☷" "☯" "☭"))
  (setq org-ellipsis " ▼ ")
  (setq org-src-fontify-natively t)
  )

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)


(provide 'setup-org)
;;; setup-org.el ends here
