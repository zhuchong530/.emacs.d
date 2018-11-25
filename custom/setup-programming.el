;;; Package -- Programming stuff
;;; Commentary:
;;; Code:

(use-package cc-mode
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
  :mode (("\\.h\\(h?\\|x\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'" . c-mode)
         ("\\.mm\=\\'" . c++-mode))
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
  :hook ((c-mode-common emacs-lisp-mode) . eldoc-mode)
  )

;; company
;; Modular text completion framework
(use-package company
  :ensure t
  :defer 5
  :diminish ""
  :bind ("C-." . company-complte)
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config (progn
            (setq company-tooltip-limit 20 ;bigger popup window
                  company-idle-delay .1     ;decrease delay before autocompletion popup shows
                  company-selection-wrap-around t
                  company-minimum-prefix-length 1
                  company-show-numbers t
                  company-dabbrev-downcase nil
                  company-transformers '(company-sort-by-occurrence))
            (setq compan-begin-commands '(self-insert-command)) ;start autocompletion only after typing
            (setq company-backends
                  '(company-irony company-irony-c-headers company-bbdb company-nxml company-css company-eclim
                                  company-semantic company-cmake company-capf
                                  (company-dabbrev-code company-gtags company-keywords)
                                  company-files company-dabbrev))
            (global-company-mode)
            (defun my-indent-or-complete()
              (interactive)
              (if (looking-at "\\_>")
                  (company-complete-common)
                (indent-according-to-mode))))
  )
;; Pakcage - irony
;; C/C++ minor mode powered by libclang
(use-package irony
  :config
  (progn
    ;; if irony server was never installed, install it
    (unless (irony--find-server-executable) (call-interactively #'irony-install-server))
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    ;; use compilation database first, clang_complete as fallback
    (setq-default irony-cdb-compilation-database '(irony-cdb-libclang
                                                   irony-cdb-clang-complete))
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    )
  ;; ;; replace the `completion-at-point' and `completion-symbol' bindings in
  ;; ;; irony-mode's buffers by irony-modes function
  ;; (defun my-irony-mode-hook()
  ;;   (define-key irony-mode-map [remap completion-at-point]
  ;;     'irony-completion-at-point-async)
  ;;   (define-key irony-mode-map [remap complete-symbol]
  ;;     'irony-completion-at-point-async))
  ;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  )

;; Package - company-irony
;; company-mode completion back-end for irony-mode
(use-package company-irony
  :after company-mode
  :defer t
  :ensure t
  :config
  (progn
    (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))
    ))

;; Package - company-irony-c-headers
;; Company mode backend for C/C++ header files with Irony
(use-package company-irony-c-headers
  :ensure t
  :after company-mode
  :config
  (add-to-list 'company-backends '(company-irony-c-headers company-irony))
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
;; Package - flycheck-irony
;; Flycheck: C/C++ support via Irony
(use-package flycheck-irony
  :after irony-mode
  :config
  (progn
    (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

;; Package - irony-eldoc
;; irony-mode support for eldoc-mode
(use-package irony-eldoc
  :ensure t
  :config
  (progn
    (add-hook 'irony-mode-hook #'irony-eldoc)))

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))



;; Package: projejctile
;; Manage and navigate projects in Emacs easily
(use-package projectile
  :ensure t
  :config
  (progn
    (projectile-global-mode)
    ))

;; Package: helm-projectile
;; Helm integration for Projectile
(use-package helm-projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-completion-system 'helm
        projectile-mode-line '(:eval (format " {%s}" (projectile-project-name))))
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))

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
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1)
  )

;;nasm-mode
(use-package nasm-mode
  :mode "\\.\\(nasm\\|s\\)$"
  )

;; Package go-guru
;; Integration of the Go 'guru' analysis tool into Emacs.
(use-package go-guru
  :ensure t
  :config
  (go-guru-hl-identifier-mode)
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
  )

;; Package company-go
;; company-mode backend for Go (using gocode)
(use-package company-go
  :disabled t
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-go))
  :after go-mode
  :bind (:map go-mode-map
                                        ;Godef jump key binding
              ("M-." . godef-jump)))

;; (defun setup-go-mode-compile()
;;   ;; Customize compile command to run go build
;;   (if (not (string-match "go" compile-command))
;;       (set (make-local-variable 'compile-command)
;;            "go build -v && go test -v && go vet")))

;;go-mode packages
;; REQUIREMENTS:
;; go get -u golang.org/x/tools/cmd/...
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/nsf/gocode
;; go get -u github.com/kisielk/errcheck
(use-package go-mode
  :ensure t
  :config
  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports"
        go-fontify-function-calls nil
        company-idle-delay .1
        )
  ;; Call gofmt before saving
  (add-hook 'before-save-hook #'gofmt-before-save)
  ;;(add-hook 'go-mode-hook 'setup-go-mode-compile)
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'go-mode-hook
            (lambda()
              (set (make-local-variable 'company-backends) '(company-go))
              (company-mode)))
  :bind
  (:map go-mode-map
        ("M-." . go-guru-definition)
        ("C-c d" . godoc-at-point)
        ("C-c g" . godoc)
        ("C-c h" . go-guru-hl-identifier)
        )
  :mode "\\.go\\'"
  )
(use-package go-errcheck)

(use-package go-add-tags)

;; go-eldoc packages
(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  )

;; (use-package flycheck-gometalinter
;;   :ensure t
;;   :config
;;   (progn
;;     (flycheck-gometalinter-setup))
;;   ;; skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
;;   (setq flycheck-gometalinter-vendor t)
;;   ;; only show errors
;;   (setq flycheck-gometalinter-errors-only t)
;;   ;; only run fast linters
;;   (setq flycheck-gometalinter-fast t)
;;   ;; use in tests files
;;   (setq flycheck-gometalinter-test t)
;;   ;; disable linters
;;   (setq flycheck-gometalinter-disable-linters '("gotype" "gocyclo"))
;;   ;; Only enable selected linters
;;   (setq flycheck-gometalinter-disable-all t)
;;   ;; Only enable selected linters
;;   (setq flycheck-gometalinter-enable-linters '("golint"))
;;   ;; Set different deadline (default: 5s)
;;   (setq flycheck-gometalinter-deadline "10s"))


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


(provide 'setup-programming)
;;; setup-programming.el ends here
