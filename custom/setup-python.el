;;;replace the web-mode with multi-web-mode at 2014.03.22
(use-package multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)
;;js2-mode --- improved mode on editting .js
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; rainbow mode for display the color
(use-package rainbow-mode
  :ensure t
  :mode "\\.css\\'"
  )

;; zencoding-mode settings
(use-package zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)    ;; Auto-start on any markup modes

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
