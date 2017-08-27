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
           (setq compan-begin-commands'(self-insert-command)) ;start autocompletion only after typing
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
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; Package - flycheck-irony
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-include-path
                           (list (expand-file-name "/usr/include")))))

;; Package - irony-eldoc
(use-package irony-eldoc
  :ensure t
  :config (add-hook 'irony-mode-hook 'irony-eldoc)
  )

;; Package - company-irony-c-headers
(use-package company-irony-c-headers
  :ensure t
  )

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

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
(setq c-default-style "k&r)" ;; set style to "linux"
(setq c-basic-offset 4)
(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

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

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; Package: projejctile
(use-package projejctile
  :ensure t
  :config ((projectile-global-mode)
	   (setq projetile-enable-cachint t))
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
  (projectile-global-mode)
  (helm-projectile-on)
  )
;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)


(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)
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
(require 'nasm-mode)
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))


;;go-mode packages
(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  )
;removes all unused imports
(add-hook 'go-mode-hook '(lambda() (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
;; format the current buffer
(add-hook 'go-mode-hook '(lambda() (local-set-key (kbd "C-c C-f") 'gofmt)))
;; format the buffer when save
(add-hook 'before-save-hook 'gofmt-before-save)
;; show the go documentation for a given package
(add-hook 'go-mode-hook '(lambda() (local-set-key (kbd "C-c C-k") 'godoc)))

;; gocode autocomplete
(add-hook 'go-mode-hook '(lambda()
                           (set (make-local-variable 'company-backends) '(company-go))
                           (company-mode)))
;; go-eldoc packages
(use-package go-eldoc
  :ensure t)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(set-face-attribute 'eldoc-highlight-function-argument nil
                    :underline t :foreground "green"
                    :weight 'bold)
;; go-guru packages
(use-package go-guru
  :ensure t)
(go-guru-hl-identifier-mode)
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

;;magit package
(use-package magit
  :ensure t
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status)
         ("C-c C-g l" . magit-log-buffer-file)
         ("C-c f" . magit-grep))
  :config (magit-auto-revert-mode)
  )


(provide 'setup-programming)
