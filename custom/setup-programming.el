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
  (setq gdb-many-windows t ;; use gdb-many-windows by default
        gdb-show-main t))  ;; Non-nil means display source file containing the main routine at startup


;; function-args
(use-package function-args
  :disabled
  :ensure t
  :config (fa-config-default)
  )

;; company
(use-package company
  :ensure t
  :commands (my-indent-or-complete)
  :bind ("TAB" . my-indent-or-complete)
  :init (progn
          (setq company-global-modes '(not python-mode cpython-mode sage-mode))
          )
  :config(progn
           (setq company-tooltip-limit 20) ;bigger popup window
           (setq company-idledelay .3)     ;decrease delay before autocompletion popup shows
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

;; Package - company-irony
(use-package company-irony
  :after company-mode
  :defer t
  :ensure t
  :init
  (add-hook 'c-mode-common-hook
            (lambda()
              (when (derived-mode-p 'c-mode 'c++-mode)
                (progn
                  (add-to-list 'company-backends 'company-irony)
                  (irony-mode)))))
  )

;; Package - company-irony-c-headers
(use-package company-irony-c-headers
  :ensure t
  :after company-mode
  :config
  (add-to-list 'company-backends '(company-irony-c-headers company-irony))
  )
;; Package -flycheck
(use-package flycheck
  :ensure t
  :bind
  (("C-c e n" . flycheck-next-error)
   ("C-c e p" . flycheck-previour-error))
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  )
;; Package - flycheck-irony
(use-package flycheck-irony
  :after flycheck-mode
  :config
  (add-hook 'flycheck-mode-hook  #'flycheck-irony-setup)
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook
            (lambda () (setq flycheck-clang-include-path
                             (list (expand-file-name "/usr/include"))))))

;; Package - irony-eldoc
(use-package irony-eldoc
  :ensure t
  :config (add-hook 'irony-mode-hook 'irony-eldoc)
  )

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)


;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))



;; Package: projejctile
(use-package projectile
  :ensure t
  :config (projectile-global-mode t)
  )
;; helm-projectile
(use-package helm-projectile
  :ensure t
  :if (display-graphic-p)
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-completion-system 'helm
        projectile-mode-line '(:eval (format " {%s}" (projectile-project-name))))
  :config
  (helm-projectile-on)
  )
;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")

;; replace the `completion-at-point' and `completion-symbol' bindings in
;; irony-mode's buffers by irony-modes function
(defun my-irony-mode-hook()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;;nasm-mode
(use-package nasm-mode
  :mode "\\.\\(nasm\\|s\\)$"
  )

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
  (setq gofmt-command "goimports")
  ;; Call gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'setup-go-mode-compile)
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'go-mode-hook (lambda ()
                            (setq tab-width 4)
                            (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                            (local-set-key (kbd "C-c C-g") 'go-goto-imports)
                            (local-set-key (kbd "C-c C-k") 'godoc)

                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-go)))
  :mode ("\\.go$" . go-mode)
  )

(use-package go-errcheck)

(use-package go-add-tags)

(use-package company-go
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-go))
  :after go-mode
  :bind (:map go-mode-map
              ;Godef jump key binding
              ("M-." . godef-jump)))


;; go-eldoc packages
(use-package go-eldoc
  :init (add-hook 'go-mode-hook 'go-eldoc-setup)
  )

(use-package flycheck-gometalinter
  :ensure t
  :config
  (progn
    (flycheck-gometalinter-setup))
  ;; skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
  (setq flycheck-gometalinter-vendor t)
  ;; only show errors
  (setq flycheck-gometalinter-errors-only t)
  ;; only run fast linters
  (setq flycheck-gometalinter-fast t)
  ;; use in tests files
  (setq flycheck-gometalinter-test t)
  ;; disable linters
  (setq flycheck-gometalinter-disable-linters '("gotype" "gocyclo"))
  ;; Only enable selected linters
  (setq flycheck-gometalinter-disable-all t)
  ;; Only enable selected linters
  (setq flycheck-gometalinter-enable-linters '("golint"))
  ;; Set different deadline (default: 5s)
  (setq flycheck-gometalinter-deadline "10s"))

;; go-guru packages
(use-package go-guru
  :ensure t
  :config
  (go-guru-hl-identifier-mode)
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
  )

(defun setup-go-mode-compile()
  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))


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
