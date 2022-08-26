;;; Package -- Summary
;;; Commentary:
;;; Code:

(use-package go-mode
  :mode (("\\.go\\'" . go-mode))
  ;; :init (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  )


(use-package go-eldoc
  :ensure t
  :defer
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  )
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))


(provide 'setup-go)
;;; setup-go.el ends here
