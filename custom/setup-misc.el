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

;; load-theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (load-theme 'badger t)                  ;dark theme, grey background
;; (load-theme 'grandshell t)              ;dark theme, balck background
(load-theme 'dracula t)                ;dark theme, purple background
;; (load-theme 'tomorrow-night-paradise t)    ;dark theme, black background
;; (load-theme 'ujelly t)
;; (load-theme 'autumn-light t)            ;light theme
;; (load-theme 'infodoc t)                 ;light theme

;;smart-mode-line
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

;;; markdown mode
(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown\\'" . markdown-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("README\\.md\\'" . gfm-mode))
  )
;; Package cmake-mode
;; major-mode for editing CMake sources
(use-package cmake-mode
  :mode ("CMakeLists.txt" "\\.cmake\\'")
  )
;; Package cmake-ide
;; Calls CMake to find out include paths and other compiler flags
(use-package cmake-ide
  :defer t
  :ensure t
  :init
  (cmake-ide-setup)
  )


(use-package gud
  :commands gud-gdb
  :bind (("<f9>" . gud-cont)
         ("<f10>" . gud-next)
         ("<f11>" . gud-step)
         ("S-<f11>" . gud-finish))
  :init
  (defun show-debugger ()
    (interactive)
    (let ((gud-buf
           (catch 'found
             (dolist (buf (buffer-list))
               (if (string-match "\\*gud-" (buffer-name buf))
                   (throw 'found buf))))))
      if (gud-buf
          (switch-to-buffer-other-window gud-buf)
          (call-interactively 'gud-gdb))))
)

(provide 'setup-misc)
;;; setup-misc.el ends here
