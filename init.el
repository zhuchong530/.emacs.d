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
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
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
(setq use-package-verbose nil)

;; add the module path
(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path (concat user-emacs-directory "custom"))
(load "setup-sys")
(load "setup-helm")
(load "setup-helm-gtags")
(load "setup-editing")
(load "setup-misc")
(load "setup-python")
(load "setup-programming")
;; On MacOS?
(when (eq system-type 'darwin)
  (load "setup-osx"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "2eb22fdddfd785dff3d1a6a851314cbaea36b128ff336e60ba73cf0d3aaa6b1f" default)))
 '(delete-selection-mode nil)
 '(package-selected-packages
   (quote
    (bug-hunter diminish symon web-mode flycheck-go projejctile zygospore ws-butler which-key volatile-highlights use-package undo-tree try tabbar switch-window swiper smartparens smart-mode-line rainbow-mode python-mode powerline nasm-mode multi-web-mode markdown-mode magit js2-mode jedi irony-eldoc iedit highlight-symbol helm-swoop helm-projectile helm-gtags helm-descbinds helm-ag golden-ratio go-guru go-errcheck go-eldoc go-add-tags ggtags function-args flycheck-irony flycheck-gometalinter exec-path-from-shell elpy duplicate-thing dtrt-indent disaster company-irony-c-headers company-irony company-go company-c-headers comment-dwim-2 color-theme clean-aindent-mode anzu ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
