;;; Package -- main init file
;;; Commentary:
;;; My GNU Emacs configuration
;;; code:

;; emacs version must >= 25
(when (version< emacs-version "25")
  (error "Requires at least GNU Emacs 25, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)
;package manager
(require 'package)
(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)


(when (not package-archive-contents)
  (package-refresh-contents))

;; bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; load use-package, used for loading packages everywhere else
(require 'use-package)
(setq use-package-always-ensure t)
;; Set to t to debug package loading
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

;; add the module path
(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path (concat user-emacs-directory "custom"))
(load "setup-sys")
(load "setup-editing")
(load "setup-deffunc")
(load "setup-misc")
(load "setup-helm")
(load "setup-company")
(load "setup-lsp")
(load "setup-programming")
(load "setup-python")
(load "setup-go")
(load "setup-web")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f8cf128fa0ef7e61b5546d12bb8ea1584c80ac313db38867b6e774d1d38c73db" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" "8e959d5a6771b4d1e2177263e1c1e62c62c0f848b265e9db46f18754ea1c1998" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "065da018725a9c72ee0358cb8e52bedfd402b222b971ba39b21a17bba84b0626" "4aafea32abe07a9658d20aadcae066e9c7a53f8e3dfbd18d8fa0b26c24f9082c" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "a3b6a3708c6692674196266aad1cb19188a6da7b4f961e1369a68f06577afa16" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "2eb22fdddfd785dff3d1a6a851314cbaea36b128ff336e60ba73cf0d3aaa6b1f" default))
 '(delete-selection-mode nil)
 '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
 '(package-selected-packages
   '(company-posframe all-the-icons company-tabnine go-eldoc rainbow-delimiters go-mode company-box bug-hunter undo-tree lsp-python-ms helm-lsp lsp-mode lsp-ui company-jedi doom-modeline rainbow-fart zeal-at-point cmake-ide cmake-mode diminish symon web-mode flycheck-go projejctile zygospore ws-butler which-key volatile-highlights use-package try tabbar switch-window swiper smartparens rainbow-mode python-mode powerline nasm-mode multi-web-mode magit js2-mode jedi iedit highlight-symbol helm-swoop helm-projectile helm-gtags helm-descbinds helm-ag golden-ratio function-args exec-path-from-shell elpy duplicate-thing dtrt-indent company-c-headers comment-dwim-2 color-theme clean-aindent-mode anzu ace-jump-mode)))

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
