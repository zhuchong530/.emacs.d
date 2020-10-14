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
;; using elpy instead jedi
(use-package elpy
  :ensure t
  :after python
  :commands (elpy-enable)
  :init
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
  ;; Use flycheck instead of flymake
  (when (load "flycheck" t t)
    (setq elpy-module (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  ;; (setq elpy-default-minor-mode '(elpy-module-company
  ;;                                 elpy-module-eldoc
  ;;                                 elpy-module-flycheck
  ;;                                 elpy-module-pyvenv
  ;;                                 elpy-module-yasnippet
  ;;                                 elpy-module-django
  ;;                                 elpy-module-sane-defaults))
  ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  ;; (flycheck-python-flake8-executable "/usr/bin/flake8-3.4")
  (add-hook 'python-mode-hook 'elpy-mode)
  ;; (setq python-check-command "flake8")
  (setq elpy-rpc-backend "jedi")
  ;; (setq elpy-use-cpython "/usr/bin/python3")
  (setq elpy-rpc-python-command "python3")
  ;; (setq python-shell-interpreter "/usr/bin/python3")
  )


(provide 'setup-python)
;;; setup-python.el ends here
