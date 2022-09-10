;;; Package -- Summary
;;; Commentary:
;;; Code:

;; Package: yasnippet
;; Yet another snippet extension for Emacs
(use-package yasnippet
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))


;; posframe - needed by lsp-bridge
(use-package posframe)
;; lsp-bridge
(use-package lsp-bridge
  :after (markdown-mode)
  :load-path "~/.emacs.d/elpa/lsp-bridge"
  :bind
  (:map lsp-bridge-mode-map
        ("M-." . lsp-bridge-find-def)
        ("M-," . lsp-bridge-return-from-def)
        ("M-?" . lsp-bridge-find-references)
        ("M-i" . lsp-bridge-lookup-documentation)
        ("M-n" . lsp-bridge-popup-documentation-scroll-up)
        ("M-p" . lsp-bridge-popup-documentation-scroll-down)
        ("s-C-n" . lsp-bridge-jump-to-next-diagnostic)
        ("s-C-p" . lsp-bridge-jump-to-prev-diagnostic))
  :config
  (setq lsp-bridge-auto-format-code-idle 5)
  (setq lsp-bridge-enable-auto-format-code t)
  (setq lsp-bridge-enable-log nil)
  (setq lsp-bridge-enable-signature-help t)
  (setq lsp-bridge-completion-provider 'corfu)
  )

(global-lsp-bridge-mode)
(yas-global-mode 1)

(message "loaded lsp-bridge")

(provide 'setup-lsp)
;;; setup-lsp.el ends here
