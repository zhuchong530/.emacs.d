;;; Package -- misc stuff
;;; Commentary:
;;; Code:

;; Save backup files in a dedicated directory
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))
(desktop-save-mode 1)

;; highlight-symbol
;; automatic and manual symbol highlighting
(use-package highlight-symbol
  :ensure t
  :bind(("C-<f3>" . highlight-symbol)
        ("<f3>" . highlight-symbol-next)
        ("S-<f3>" . highlight-symbol-prev)
        ("M-<f3>" . highlight-symbol-query-replace)
        )
  )

;; Package: tabbar
;; Display a tab bar in the header line
(use-package tabbar
  :ensure t
  :demand
  :config (tabbar-mode t)
  :bind (("C-M-9" . tabbar-backward-group)
         ("C-M-0" . tabbar-forward-group)
         ("C-9" . tabbar-backward)
         ("C-0". tabbar-forward))
  )

;; Package ace-jump-mode
;; a quick cursor location minor mode for emacs
(use-package ace-jump-mode
  :ensure t
  :bind ("C-c <SPC>" . ace-jump-mode)
  )

;; Package dash-at-point
(use-package dash-at-point
  :ensure t
  :defer 15
  :config
  (autoload 'dash-at-point "dash-at-point"
    "Search the word at point with Dash." t nil)
  (global-set-key "\C-cd" 'dash-at-point)
  ;; (global-set-key "\C-ce" 'dash-at-point-docset)  ;; key coflict with flycheck, disable first
  (add-to-list 'dash-at-point-mode-alist '(python-mode . "python3"))
  )

;; Package golden-ratio
;; Automatic resizing of Emacs windows to the golden ratio
(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
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
                                             "dired-mode"))
  (golden-ratio-mode 1)
  )

;; load-theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (load-theme 'dracula t)                ;dark theme, purple background
;; (load-theme 'monokai t)                 ;; dark theme monokai

;; Package: smart-mode-line
;; A color coded smart mode-line
;; (use-package smart-mode-line
;;   :ensure t
;;   :defer 10
;;   :config
;;   (sml/setup)
;;   (sml/apply-theme 'respectful)
;;   )

;; Package all-the-icons
;;
(use-package all-the-icons)

;; Package all-the-icons-dired
;;
(load "all-the-icons-dired.el")
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Package doom-modeline
;; A minimal and modern modeline
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-python-execuable "python")
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-icon t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-persp-name t)
  ;; (setq doom-modeline-github t)         ;requires "ghub" packages. not installed
  )

;; Package doom-themes
(use-package doom-themes
  :ensure t
  :defer t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  )
(load-theme 'doom-dracula t)

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
(use-package tramp
  :ensure t
  )

;; Package: markdown mode
;; Major mode for Markdown-formatted text
(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown")
  )

(provide 'setup-misc)
;;; setup-misc.el ends here
