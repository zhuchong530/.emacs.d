;;; Package -- Summary
;;; Commentary:
;;; Code:

;; (defun lsp-go-install-save-hooks()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t)
;;   )
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; (use-package go-mode
;;   :mode (("\\.go\\'" . go-mode))
;;   ;; :init (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
;;   )


(use-package go-eldoc
  :ensure t
  :defer
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  )
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))

(provide 'setup-go)
;;; setup-go.el ends here
