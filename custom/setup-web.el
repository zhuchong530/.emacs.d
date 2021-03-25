;;; Package -- Summary
;;; Commentary:
;;; Code:

;; Package web-mode
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.api\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("/some/react/path/.*\\.js[x]?\\'" . web-mode))
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\."))
        )
  (setq web-mode-content-types-alist
        '(("json" . "/some/path/.*\\.api\\'")
          ("xml"  . "/other/path/.*\\.api\\'")
          ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-column-highlight t)
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    )
  (add-hook 'web-mode-hook  'my-web-mode-hook)

  )

;;js2-mode --- improved mode on editting .js
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :bind (("C-c ! n" . js2-next-error))
  :init
  (progn
    (setq-default js2-basic-offset 4)
    (setq-default js2-strict-trailing-comma-warning t)
    (setq-default js2-global-externs
                  '("module"
                    "exports"
                    "require"
                    "process"
                    "setTimeout"
                    "clearTimeout"
                    "setInterval"
                    "clearInterval"
                    "window"
                    "location"
                    "__dirname"
                    "console"
                    "JSON"))
    (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode)))
  )


(provide 'setup-web)
;;; setup-web.el ends here
