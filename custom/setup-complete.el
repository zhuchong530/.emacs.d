;;; Package -- Summary
;;; Commentary:
;;; Code:


(use-package corfu
  :after (orderless lsp-bridge)
  :bind (:map corfu-map
              ("M-n" . corfu-next)
              ("M-p" . corfu-previous)
              ("TAB" . corfu-insert))
  :custom
  ;; (corfu-cycle t)
  ;; auto completion
  (corfu-auto t)
  (corfu-auto-delay 0.25)
  (corfu-auto-prefix 2)
  (corfu-max-width 110)
  (corfu-preview-current nil)
  (corfu-echo-documentation t)
  :config (global-corfu-mode))

;; Package: yasnippet
;; Yet another snippet extension for Emacs
(use-package yasnippet
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :hook ((prog-mode org-mode) . yas-minor-mode)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1)
  )


;; posframe - needed by lsp-bridge
(use-package posframe)
;; lsp-bridge
;; (use-package lsp-bridge
;;   :load-path "~/.emacs.d/elpa/lsp-bridge"
;;   :bind
;;   (:map lsp-bridge-mode-map
;;         ("M-." . lsp-bridge-find-def)
;;         ("M-," . lsp-bridge-return-from-def)
;;         ("M-?" . lsp-bridge-find-references)
;;         ("M-i" . lsp-bridge-lookup-documentation)
;;         ("M-n" . lsp-bridge-popup-documentation-scroll-up)
;;         ("M-p" . lsp-bridge-popup-documentation-scroll-down)
;;         ("s-C-n" . lsp-bridge-jump-to-next-diagnostic)
;;         ("s-C-p" . lsp-bridge-jump-to-prev-diagnostic))
;;   :config
;;   (setq lsp-bridge-auto-format-code-idle 5)
;;   (setq lsp-bridge-enable-auto-format-code t)
;;   (setq lsp-bridge-enable-log nil)
;;   (setq lsp-bridge-enable-signature-help t)
;;   (setq lsp-bridge-completion-provider 'corfu)
;;   )

(add-to-list 'load-path "~/.emacs.d/elpa/lsp-bridge")
(require 'lsp-bridge)
(global-lsp-bridge-mode)



;; (use-package cape
;;   :bind (("C-c p p" . completion-at-point) ;capf
;;          ("C-c p t" . completion-tag)      ;etags
;;          ("C-c p d" . cape-dabbrev)        ;or dabbrev-completion
;;          ("C-c p h" . cape-history)
;;          ("C-c p f" . cape-file)
;;          ("C-c p k" . cape-keyword)
;;          ("C-c p s" . cape-symbol)
;;          ("C-c p a" . cape-abbrev)
;;          ("C-c p i" . cape-ispell)
;;          ("C-c p l" . cape-line)
;;          ("C-c p w" . cape-dict)
;;          ;; ("C-c p \\" . cape-tex)
;;          ;; ("C-c p _" . cape-tex)
;;          ;; ("C-c p ^" . cape-tex)
;;          ;; ("C-c p &" . cape-sgml)
;;          ;; ("C-c p r" . cape-rfc1345)
;;          )
;;   :init
;;   ;; Add completion-at-point-function, used by completion-at-point
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   )


(provide 'setup-complete)
;;; setup-company.el ends here
