;;; Package -- misc stuff
;;; Commentary:
;;; Code:

;;linum-mode
(use-package linum
  :config
  (setq linum-format "%5d")
  (global-linum-mode 1)
  )

;; highlight-symbol
(use-package highlight-symbol
  :bind(("C-<f3>" . highlight-symbol)
        ("<f3>" . highlight-symbol-next)
        ("S-<f3>" . highlight-symbol-prev)
        ("M-<f3>" . highlight-symbol-query-replace)))

;;package ace-jump-mode
(use-package ace-jump-mode
  :bind ("C-c <SPC>" . ace-jump-mode)
  )

;;package golden-ratio
(use-package golden-ratio
  :init (golden-ratio-mode 1)
  :config (setq golden-ratio-exclude-modes '("ediff-mode"
                                             "gud-mode"
                                             "gdb-locals-mode"
                                             "gdb-registers-mode"
                                             "gdb-breakpoints-mode"
                                             "gdb-threads-mode"
                                             "gdb-frames-mode"
                                             "gdb-inferior-io-mode"
                                             "gud-mode"
                                             "gdb-inferior-io-mode"
                                             "gdb-disassembly-mode"
                                             "gdb-memory-mode"
                                             "IELM"
                                             ;; "eshell-mode" "dired-mode"))
                                             )))

;; load-theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (load-theme 'dracula t)                ;dark theme, purple background
;; (load-theme 'doom-dark+ t)
;; (load-theme 'tomorrow-night-paradise t)    ;dark theme, black background

(use-package doom-themes
  :config
  (load-theme 'doom-dracula t)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; doom-modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  ;; 不显示换行和编码(节省空间)
  (setq doom-modeline-buffer-encoding t))

;; package centaur-tabs
(use-package centaur-tabs
  :demand
  :hook (emacs-startup . centaur-tabs-mode)
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  :init
  (setq centaur-tabs-set-bar t
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker " ● "
        centaur-tabs-style "rounded")
  :bind
  ("C-9" . centaur-tabs-backward)
  ("C-0" . centaur-tabs-forward)
  )

(use-package all-the-icons
  :if (display-graphic-p)
  )

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  )

;; rainbow mode for display the color
(use-package rainbow-mode
  :diminish
  :hook (prog-mode . rainbow-mode))


(use-package rainbow-delimiters
  :init (progn (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "chartreuse3")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "DodgerBlue1")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "DarkOrange2")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "deep pink")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "medium orchid")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "turquoise")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "lime green")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "gold")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "cyan")
  (set-face-bold 'rainbow-delimiters-depth-1-face "t")
  (set-face-bold 'rainbow-delimiters-depth-2-face "t")
  (set-face-bold 'rainbow-delimiters-depth-3-face "t")
  (set-face-bold 'rainbow-delimiters-depth-4-face "t")
  (set-face-bold 'rainbow-delimiters-depth-5-face "t")
  (set-face-bold 'rainbow-delimiters-depth-6-face "t")
  (set-face-bold 'rainbow-delimiters-depth-7-face "t")
  (set-face-bold 'rainbow-delimiters-depth-8-face "t")
  (set-face-bold 'rainbow-delimiters-depth-9-face "t")
  )

(use-package zeal-at-point
  :defer 10
  :bind ("C-c d" . 'zeal-at-point)
  :config
  (add-to-list 'zeal-at-point-mode-alist '(python-mode . "python3"))
  (add-to-list 'zeal-at-point-mode-alist '(c-mode . "C"))
  (add-to-list 'zeal-at-point-mode-alist '(c++-mode . "C++"))
  (add-to-list 'zeal-at-point-mode-alist '(cc-mode . ("C" "C++")))
  (add-to-list 'zeal-at-point-mode-alist '(go-mode . "Go"))
  )

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command)
  )


;; Marginalia
;; Enhances the minibuffer completions with additional informations
(use-package marginalia
  :custom (marginalia-annotators
           '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))

;; Orderless
;; Controls the sorting of the minibuffer completions
(use-package orderless
  :custom ((completion--styles '(orderless))
           (completion-category-defaults nil)
           (completion--category-override '((file (style . (partial-completion)))))))


(use-package vertico
  :config (vertico-mode))

(use-package nyan-mode
  :custom
  (nyan-cat-face-number 3)
  (nyan-animate-nyancat t)
  :config (nyan-mode 1))

(use-package youdao-dictionary
  :config
  (setq url-automatic-caching t)
  )
(global-set-key (kbd "C-c k") 'youdao-dictionary-search-at-point-posframe)

;;tramp
;;;;;;;;;;;;;;;;;;;;;;;;;tramp setting;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;Enable you to edit the file which on the REMOTE machines;;;;;
;;;  tramp support many protocols:like ftp,ssh,etc.
;;;USAGE:
;;   /host:filepath
;;   /user@host:filepath
;;   /user@host#port:filepath
;;   /method:user@host:filepath
;;   /method:user@host#port:filepath
;;;;;; method stand for which protocol you want to use.
;;;;;;  host stand for the remote hostname/Ip Address
(use-package tramp)


;; Package markdown-mode
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'setup-misc)
;;; setup-misc.el ends here
