;;;replace the web-mode with multi-web-mode at 2014.03.22
(require 'multi-web-mode)
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
(require 'rainbow-mode)
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-turn-on))
;; zencoding-mode settings
;;(add-to-list 'load-path "/home/wangchang/.emacs.d/site-lisp/zencoding")
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)    ;; Auto-start on any markup modes
;; ;;;;;;;;python programming settings;;;;;;;;;;;;;;;;;;;;;;;;;
;;(add-to-list 'load-path py-install-directory)
(add-to-list 'auto-mode-alist '("\.py\'" . python-mode))
;;jedi
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional

(provide 'setup-python)
