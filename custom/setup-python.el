;;; Package -- Summary
;;; Commentary:
;;; Code:

(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  ;; :interpreter ("python3" . python-mode)
  :init
  (add-hook 'python-mode-hook 'hs-minor-mode)
  :config
  (setq python-shell-interpreter "python3")
  )
;; python3.3 build-in virtualenv environments
(use-package pyvenv
  :demand
  :config
  (pyvenv-mode 1)
  )
(setenv "WORKON_HOME" "/home/wangchang/Programme/pythonCode")

;;Package lsp-python-ms
(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp)))
  ;; :init (setq lsp-python-ms-executable "~/.emacs.d/pylsms/Microsoft.Python.LanguageServer")
  ;; :init (setq lsp-python-ms-executable "/usr/bin/mspyls")
  :init (setq lsp-python-ms-auto-install-server t)
  )

(provide 'setup-python)
;;; setup-python.el ends here
