;;; Package -- main init file
;;; Commentary:
;;; Cathy.chang's GNU Emacs configuration
;;; code:


(when (version< emacs-version "25")
  (error "Requires at least GNU Emacs 25, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)                      ;package manager
(setq package-enable-at-startup nil)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("marmalade" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/marmalade/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; load use-package, used for loading packages everywhere else
(require 'use-package)
;; Set to t to debug package loading
;;(setq use-package-verbose nil)
(setq use-package-verbose t)

;; add the module path
(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path (concat user-emacs-directory "custom"))
(load "setup-sys")
(load "setup-misc")
(load "setup-editing")
(load "setup-programming")
(load "setup-python")
(load "setup-helm")
(load "setup-helm-gtags")
(load "setup-osx")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "f8cf128fa0ef7e61b5546d12bb8ea1584c80ac313db38867b6e774d1d38c73db" "4bcdfc98cf64ce6145684dc8288fd87489cfa839e07f95f6c791d407624d04f8" "f5a7e07642decb17b03483af7c44e93353d2b128de403bf301651954c628c0ab" "4aafea32abe07a9658d20aadcae066e9c7a53f8e3dfbd18d8fa0b26c24f9082c" "0b6cb9b19138f9a859ad1b7f753958d8a36a464c6d10550119b2838cedf92171" "e068203104e27ac7eeff924521112bfcd953a655269a8da660ebc150c97d0db8" "95b0bc7b8687101335ebbf770828b641f2befdcf6d3c192243a251ce72ab1692" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "2eb22fdddfd785dff3d1a6a851314cbaea36b128ff336e60ba73cf0d3aaa6b1f" default)))
 '(delete-selection-mode nil)
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(package-selected-packages
   (quote
    (doom-themes all-the-icons doom-modeline all-the-icons-dired ggtags rainbow-delimiters pyvenv-mode dash-at-point cmake-ide cmake-mode bug-hunter diminish symon web-mode flycheck-go projejctile ws-butler which-key volatile-highlights use-package undo-tree try tabbar switch-window swiper smartparens smart-mode-line rainbow-mode python-mode powerline nasm-mode multi-web-mode markdown-mode magit js2-mode jedi irony-eldoc iedit highlight-symbol helm-swoop helm-projectile helm-gtags helm-descbinds helm-ag golden-ratio go-guru go-errcheck go-eldoc go-add-tags function-args flycheck-irony flycheck-gometalinter exec-path-from-shell elpy duplicate-thing dtrt-indent company-irony-c-headers company-irony company-go company-c-headers comment-dwim-2 color-theme clean-aindent-mode anzu ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
