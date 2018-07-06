;;; Package -- misc stuff
;;; Commentary:
;;; Code:

;; Save backup files in a dedicated directory
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))
(desktop-save-mode 1)

;;linum-mode
(use-package linum
  :ensure t
  :config (global-linum-mode 1)
  )

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
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (load-theme 'badger t)                  ;dark theme, grey background
;; (load-theme 'grandshell t)              ;dark theme, balck background
(load-theme 'dracula t)                ;dark theme, purple background
;; (load-theme 'monokai t)                 ;; dark theme monokai
;; (load-theme 'tomorrow-night-paradise t)    ;dark theme, black background
;; (load-theme 'ujelly t)

;; Package: smart-mode-line
;; A color coded smart mode-line
(use-package smart-mode-line
  :ensure t
  :defer 10
  :config
  (sml/setup)
  (sml/apply-theme 'respectful)
  )

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
  )

(provide 'setup-misc)
;;; setup-misc.el ends here
