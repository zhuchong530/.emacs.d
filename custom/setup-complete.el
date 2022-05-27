;;; Package -- Summary
;;; Commentary:
;;; Code:


(use-package corfu
  :bind (:map corfu-map
              ("M-n" . corfu-nex)
              ("M-p" . corfu-previous)
              ("TAB" . corfu-insert))
  :custom (corfu-cycle t)
  :config (global-corfu-mode))

;; ;; comapny-tabnine
;; ;; OpenAI completion backend
;; (use-package company-tabnine
;;   :ensure t
;;   )
;; ;; The free version of TabNine is good enough,
;; ;; and below code is recommended that TabNine not always
;; ;; prompt me to purchase a paid version in a large project.
;; (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
;;   (let ((company-message-func (ad-get-arg 0)))
;;     (when (and company-message-func
;;                (stringp (funcall company-message-func)))
;;       (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
;;         ad-do-it))))


(provide 'setup-company)
;;; setup-company.el ends here
