;;; package -- Summary:
;;; Commentary:
;;; Code:
(require 'org)
(org-babel-load-file "~/.emacs.d/config.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(fanyi devdocs transwin org-roam vertico-posframe olivetti treemacs uniquify zeal-at-point youdao-dictionary yasnippet ws-butler which-key web-mode volatile-highlights vertico use-package undo-tree toc-org rustic realgud-lldb rainbow-mode rainbow-delimiters pyvenv projectile posframe ox-reveal ox-gfm org-mime org-download org-bullets orderless ob-go nyan-mode nasm-mode marginalia magit lsp-mode js2-mode iedit highlight-symbol highlight-indent-guides helpful golden-ratio go-eldoc git-gutter+ gcmh flycheck-rust exec-path-from-shell doom-themes doom-modeline diminish consult comment-dwim-2 cmake-mode clean-aindent-mode centaur-tabs cargo all-the-icons-dired all-the-icons-completion ace-window)))

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :foreground "red" :height 7.0)))))
