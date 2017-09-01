;; main init file
;; Commentary:
;; Cathy.chang's GNU Emacs configuration
;;

;; code:




;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path (concat user-emacs-directory "custom"))
(require 'setup-sys)
(require 'setup-helm)
(require 'setup-helm-gtags)
(require 'setup-editing)
(require 'setup-misc)
(require 'setup-python)
(require 'setup-programming)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages
   (quote
    (flycheck-go projejctile zygospore zencoding-mode ws-butler which-key volatile-highlights use-package undo-tree try tabbar switch-window swiper smartparens smart-mode-line rainbow-mode python-mode powerline nasm-mode multi-web-mode markdown-mode magit js2-mode jedi irony-eldoc iedit highlight-symbol helm-swoop helm-projectile helm-gtags helm-descbinds helm-ag golden-ratio go-guru go-errcheck go-eldoc go-add-tags ggtags function-args flycheck-irony flycheck-gometalinter exec-path-from-shell elpy duplicate-thing dtrt-indent disaster company-irony-c-headers company-irony company-go company-c-headers comment-dwim-2 color-theme clean-aindent-mode anzu ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
