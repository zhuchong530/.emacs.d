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
  ;; (add-to-list 'auto-mode-alist '("/some/react/path/.*\\.js[x]?\\'" . web-mode))
  (setq web-mode-engines-alist
        '(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\."))
        )
  ;; (setq web-mode-content-types-alist
  ;;       '(("json" . "/some/path/.*\\.api\\'")
  ;;         ("xml"  . "/other/path/.*\\.api\\'")
  ;;         ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-column-highlight t)
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 4)
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
    (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))
    (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
    )
  )
;; rainbow mode for display the color
(use-package rainbow-mode
  :ensure t
  :mode "\\.css\\'"
  )

(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :init
  (add-hook 'python-mode-hook 'hs-minor-mode)
  :config
  (setq python-indent-offset 4)
  )

;; using elpy instead jedi
(use-package elpy
  :ensure t
  :after python
  :commands (elpy-enable)
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
  (elpy-enable)
  (add-hook 'python-mode-hook 'elpy-mode)
  (setq python-check-command "flake8")
  (setq elpy-rpc-backend "jedi")
  (setq elpy-use-cpython "/usr/local/bin/python3")
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
  (pyvenv-mode 1)
  )

;; python3.3 build-in virtualenv environments
(use-package pyenv-mode
  :init
  (setenv "WORKON_HOME" "~/Programme/PythonEnvs")
  :config
  (pyvenv-mode 1)
  )

(provide 'setup-python)
;;; setup-python.el ends here
