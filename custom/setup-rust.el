;;; Package -- Summary
;;; Commentary:
;;; Code:


(use-package rustic
  :ensure t
  :after lsp-mode
  :init (setq rustic-lsp-server 'rust-analysis
              rustic-format-on-save t)
  :config
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  )

(use-package cargo
  :ensure t
  :config (add-hook 'rust-mode-hook 'cargo-minor-mode)
  )

(use-package flycheck-rust
  :ensure t
  :config (add-hook 'rust-mode-hook #'flycheck-rust-setup)
  )



(provide 'setup-rust)
;;; setup-rust.el ends here
