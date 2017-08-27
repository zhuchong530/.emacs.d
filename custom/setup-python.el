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
  :config (elpy-enable)
  )
(setq elpy-use-cpython "/usr/bin/python3")
(setq elpy-rpc-python-command "python3")
(setq elpy-rpc-backend "jedi")


(provide 'setup-python)
