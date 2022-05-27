;;; Package -- Summary
;;; Commentary:
;;; Code:


(use-package corfu
  :after orderless
  :bind (:map corfu-map
              ("M-n" . corfu-nex)
              ("M-p" . corfu-previous)
              ("TAB" . corfu-insert))
  :custom
  ;; (corfu-cycle t)
  (corfu-auto t)
  (corfu-max-width 110)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 1)
  (corfu-preview-current nil)
  (corfu-echo-documentation t)
  :config (global-corfu-mode))

(use-package corfu-doc
  :hook (corfu-mode-hook . corfu-doc-mode)
  )


(use-package cape
  :bind (("C-c p p" . completion-at-point) ;capf
         ("C-c p t" . completion-tag)      ;etags
         ("C-c p d" . cape-dabbrev)        ;or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ;; ("C-c p \\" . cape-tex)
         ;; ("C-c p _" . cape-tex)
         ;; ("C-c p ^" . cape-tex)
         ;; ("C-c p &" . cape-sgml)
         ;; ("C-c p r" . cape-rfc1345)
         )
  :init
  ;; Add completion-at-point-function, used by completion-at-point
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  )


(provide 'setup-complete)
;;; setup-company.el ends here
