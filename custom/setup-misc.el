;; GROUP: misc

;; Save backup files in a dedicated directory
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))
(desktop-save-mode 1)

;;linum-mode
(use-package linum
  :ensure t
  :config (global-linum-mode 1)
  )

;; highlight-symbol
(use-package highlight-symbol
  :ensure t
  :bind(("C-<f3>" . highlight-symbol)
	("<f3>" . highlight-symbol-next)
	("S-<f3>" . highlight-symbol-prev)
	("M-<f3>" . highlight-symbol-query-replace)
	)
  )

;;package tabbar
(use-package tabbar
  :ensure t
  :demand
  :config (tabbar-mode t)
  :bind (("C-M-9" . tabbar-backward-group)
	 ("C-M-0" . tabbar-forward-group)
	 ("C-9" . tabbar-backward)
	 ("C-0". tabbar-forward))
  )

;;package ace-jump-mode
(use-package ace-jump-mode
  :ensure t
  :bind ("C-c <SPC>" . ace-jump-mode)
  )

;;package golden-ratio
(use-package golden-ratio
  :ensure t
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
                                   "eshell-mode" "dired-mode"))
  )

;;color-theme
(use-package color-theme
  :disabled
  :ensure t
  :config ((color-theme-initialize)
	   (setq color-theme-is-global t)
	   (color-theme-dark-laptop)
	   )
  )
;; load-theme
(add-to-list 'custom-theme-load-path "/home/wangchang/.emacs.d/themes")
;; (load-theme 'badger t)
(load-theme 'grandshell t)

;;smart-mode-line
(use-package smart-mode-line
  :ensure t
  )
(setq sml/no-confirm-load-theme t)
(sml/setup)
(set sml/theme 'dark)

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

;;; markdown mode
(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown\\'" . markdown-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("README\\.md\\'" . gfm-mode))
  )


(provide 'setup-misc)
