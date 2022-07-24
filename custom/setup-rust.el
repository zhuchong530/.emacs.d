;;; Package -- Summary
;;; Commentary:
;;; Code:


(use-package rust-mode)


(use-package rustic
  :ensure t
  :after lsp-mode
  :config
  (setq rustic-format-on-save t)
  )

(defun rustic-mode-auto-save-hook ()
  "Enable auto-saving in rustic-mode buffers."
  (when buffer-file-name
    (setq-local compilation-ask-about-save nil)))
(add-hook 'rust-mode-hook 'rustic-mode-auto-save-hook)

(use-package cargo
  :ensure t
  :hook
  (rust-mode . cargo-minor-mode)
  )

(use-package flycheck-rust
  :ensure t
  :config (add-hook 'rust-mode-hook #'flycheck-rust-setup)
  )



(provide 'setup-rust)
;;; setup-rust.el ends here
