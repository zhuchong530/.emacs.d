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
  :commands js2-mode
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
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; rainbow mode for display the color
(use-package rainbow-mode
  :ensure t
  :mode "\\.css\\'"
  )


;; python3.3 build-in virtualenv environments
(use-package pyvenv)
;; using elpy instead jedi
(use-package elpy
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  :bind (:map elpy-mode-map
              ("<M-left>" . nil)
              ("<M-right>" . nil)
              ("<M-S-left>" . elpy-nav-indent-shift-left)
              ("<M-S-right>" . elpy-nav-indent-shift-right)
              ("M-." . elpy-goto-definition)
              ("M-," . pop-tag-mark)
              )
  :config
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  flycheck-python-flake8-executable "/usr/bin/flake8-3.4"
  (setq elpy-rpc-backend "jedi")
  (setq elpy-use-cpython "/usr/bin/python3")
  (setq elpy-rpc-python-command "python3")
  )
(use-package python
  :mode ("\\.py" . python-mode)
  :config
  (setq python-indent-offset 4)
  (elpy-enable)
  )

(provide 'setup-python)
