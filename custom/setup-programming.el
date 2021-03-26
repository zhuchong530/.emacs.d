;;; Package -- Programming stuff
;;; Commentary:
;;; Code:

(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'" . c-mode)
         ("\\.mm\\'" . c-mode))

  :config
  ;; Available C style:
  ;; “gnu”: The default style for GNU projects
  ;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
  ;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
  ;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
  ;; “stroustrup”: What Stroustrup, the author of C++ used in his book
  ;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
  ;; “linux”: What the Linux developers use for kernel development
  ;; “python”: What Python developers use for extension modules
  ;; “java”: The default style for java-mode (see below)
  ;; “user”: When you want to define your own style
  (setq c-default-style "k&r") ;; set style to "k&r"
  (setq c-basic-offset 4)
  (global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET
  )

;; Package function-args
;; C++ completion for GNU Emacs
(use-package function-args
  :disabled
  :ensure t
  :config (fa-config-default)
  )

;; Package eldoc
(use-package eldoc
  :diminish
  :hook ((c-mode-common) . eldoc-mode)
  )

;; Package -flycheck
;; On-the-fly syntax checking
(use-package flycheck
  :ensure t
  :defer t
  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error)
  :init (global-flycheck-mode)
  :bind
  (("C-c e n" . flycheck-next-error)
   ("C-c e p" . flycheck-previour-error)
   ("C-c e l" . flycheck-list-errors))
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(save-mode-enabled))
    (setq flycheck-standard-error-navigation nil)
    ;;flycheck errors on a tooltip(doesn't work on console)
    (when (display-graphic-p (selected-frame))
      (eval-after-load 'flycheck
        '(custom-set-variables
          '(flycheck-display-errors-function
            #'flycheck-pos-tip-error-messages))))
    )
  )

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)
;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))



;; Package: projejctile
;; Manage and navigate projects in Emacs easily
(use-package projectile
  :ensure t
  :requires (helm)
  :bind (:map projectile-mode-map
              ("C-c p" . 'projectile-command-map))
  :config
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-completion-system 'helm)
  (projectile-mode +1))

;; Package: helm-projectile
;; Helm integration for Projectile
(use-package helm-projectile
  :ensure t
  :requires (helm projectile)
  :config
  (helm-projectile-on))

;; Package zygospore
(use-package zygospore
  :ensure t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows))
  )

;; Package: yasnippet
;; Yet another snippet extension for Emacs
(use-package yasnippet
  :after prog-mode
  :defer t
  :diminish yas-minor-mode
  :bind (("C-c y d" . yas-load-directory)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y f" . yas-visit-snippet-file)
         ("C-c y n" . yas-new-snippet)
         ("C-c y t" . yas-tryout-snippet)
         ("C-c y l" . yas-describe-tables)
         ("C-c y g" . yas/global-mode)
         ("C-c y m" . yas/minor-mode)
         ("C-c y a" . yas-reload-all)
         ("C-c y x" . yas-expand))
  :bind (:map yas-keymap
              ("C-i" . yas-next-field-or-maybe-expand))
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1)
  )

;; Pakcage yasnippet-snippets
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;nasm-mode
(use-package nasm-mode
  :mode "\\.\\(nasm\\|s\\)$"
  )

;;magit package
(use-package magit
  :ensure t
  :commands magit-get-top-dir
  :bind (("C-x g s" . magit-status)
         ("C-x g f" . magit-log-buffer-file)
         ("C-x g x" . magit-checkout)
         ("C-x g c" . magit-commit)
         ("C-x g p" . magit-push)
         ("C-x g u" . magit-pull)
         ("C-x g e" . magit-ediff-resolve)
         ("C-x g r" . magit-rebase-interactive))
  :config (magit-auto-revert-mode)
  )

(use-package git-gutter+
  :ensure t
  :config
  (progn
    (global-git-gutter+-mode)))

(provide 'setup-programming)
;;; setup-programming.el ends here
