;;; Package -- Summary
;;; Commentary:
;;; Code:

;; company
;; Modular text completion framework
(use-package company
  :ensure t
  :defer 5
  :diminish ""
  :bind ("C-." . company-complte)
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config (progn
            (setq company-tooltip-limit 20 ;bigger popup window
                  company-idle-delay .1     ;decrease delay before autocompletion popup shows
                  company-selection-wrap-around t
                  company-minimum-prefix-length 1
                  company-show-numbers t
                  company-dabbrev-downcase nil
                  company-transformers '(company-sort-by-occurrence))
            (setq compan-begin-commands '(self-insert-command)) ;start autocompletion only after typing
            (setq company-backends
                  '(company-bbdb company-nxml company-css
                                  company-semantic company-cmake company-capf-irony
                                  (company-dabbrev-code company-gtags company-keywords)
                                  company-files company-dabbrev))
            (global-company-mode)
            (defun my-indent-or-complete()
              (interactive)
              (if (looking-at "\\_>")
                  (company-complete-common)
                (indent-according-to-mode))))
)
;;Package compnay-lsp
;; add compnay-lsp as a backend
(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config (push 'company-lsp company-backends)
  )


(provide 'setup-company)
;;; setup-company.el ends here
