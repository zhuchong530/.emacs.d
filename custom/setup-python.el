;;; Package -- Summary
;;; Commentary:
;;; Code:

;; Package web-mode
(use-package web-mode
  :bind (("C-c ]" . emmet-next-edit-point)
         ("C-c [" . emmet-prev-edit-point)
         ("C-c o b" . browse-url-of-file))
  :mode
  (("\\.js\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.phtml?\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.jsx$" . web-mode)
   )
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  ;;highlight enclosing tags of the element under cursor
  (setq web-mode-enable-current-element-highlight t)
  (use-package web-mode-edit-element
    :config (add-hook 'web-mode-hook 'web-mode-edit-element-minor-mode))

  ;;snippets for HTML
  (use-package emmet-mode
    :init (setq emmet-move-cursor-between-quotes t)
    :diminish (emmet-mode . " e"))
  (add-hook 'web-mode-hook 'emmet-mode)

  (defun my-web-mode-hook ()
    "Hook for `web-mode' config for company-backends."
    (set (make-local-variable 'company-backends)
         '((company-tern company-css company-web-html company-files))))
  (add-hook 'web-mode-hook 'my-web-mode-hook)

  ;; Enable JavaScript completion between <script>...</script> etc.
  (defadvice company-tern (before web-mode-set-up-ac-sources activate)
    "Set `tern-mode' based on current language before running company-tern."
    (message "advice")
    (if (equal major-mode 'web-mode)
        (let ((web-mode-cur-language
               (web-mode-language-at-pos)))
          (if (or (string= web-mode-cur-language "javascript")
                  (string= web-mode-cur-language "jsx"))
              (unless tern-mode (tern-mode))
            (if tern-mode (tern-mode -1))))))
  (add-hook 'web-mode-hook 'company-mode)

  ;; to get completion for HTML stuff
  ;; https://github.com/osv/company-web
  (use-package company-web)

  (add-hook 'web-mode-hook 'company-mode))

;;js2-mode --- improved mode on editting .js
(use-package js2-mode
  :ensure t
  :mode
  ("\\.js$" . js2-mode)
  ("\\.json$" . js2-jsx-mode)
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
  :mode ("\\.py" . python-mode)
  :interpreter ("python3" . python-mode)
  :init
  (add-hook 'python-mode-hook 'hs-minor-mode)
  :config
  )
;; using elpy instead jedi
(use-package elpy
  :init
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  :config
  (setq python-check-command "flake8")
  (setq elpy-rpc-backend "jedi")
  (setq elpy-use-cpython "/usr/local/bin/python3")
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
  ;; :bind-keymap (:map elpy-mode-map
  ;;             ("M-." . elpy-goto-definition)
  ;;             ("M-," . pop-tag-mark))
  (elpy-enable)
  (setq python-indent-offset 4))
;; python3.3 build-in virtualenv environments
(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/Programme/PythonEnvs")
  (setenv "WORKON_HOME" "~/Programme/PythonEnvs")
  :config
  (pyvenv-mode)
  :bind
  ("C-x p e" . pyenv-activate-current-project)
  )


(use-package php-mode
  :mode
  (("\\.php\\'" . php-mode))
  :config
  (add-hook 'php-mode-hook
            '(lambda ()
               (require 'company-php)
               (company-mode t)
               (add-to-list 'company-backends 'company-ac-php-backend)))
  )
(use-package phpunit
  :mode
  (("\\.php\\'" . phpunit-mode))
  )

(use-package xah-css-mode)

(provide 'setup-python)
;;; setup-python.el ends here
