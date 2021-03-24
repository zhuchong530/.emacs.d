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

;; package awesome-tab
(use-package awesome-tab
  :load-path "~/.emacs.d/site-lisp/awesome-tab"
  :config (awesome-tab-mode t)
  :bind (("C-M-9" . awesome-tab-backward-group)
         ("C-M-0" . awesome-tab-forward-group)
         ("C-9" . awesome-tab-backward-tab)
         ("C-0" . awesome-tab-forward-tab))
  )

;;package ace-jump-mode
(use-package ace-jump-mode
  :ensure t
  :bind ("C-c <SPC>" . ace-jump-mode)
  )

;;package golden-ratio
(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
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
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)                ;dark theme, purple background
;; (load-theme 'doom-dark+ t)
;; (load-theme 'tomorrow-night-paradise t)    ;dark theme, black background

;; doom-modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  )

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  )

(use-package all-the-icons)

(use-package rainbow-delimiters
  :ensure t
  :init (progn (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
  )

;; (use-package posframe)


(use-package zeal-at-point
  :defer 10
  :bind ("C-c d" . 'zeal-at-point)
  :config
  (add-to-list 'zeal-at-point-mode-alist '(python-mode . "python3"))
  (add-to-list 'zeal-at-point-mode-alist '(c-mode . "C"))
  (add-to-list 'zeal-at-point-mode-alist '(c++-mode . "C++"))
  (add-to-list 'zeal-at-point-mode-alist '(cc-mode . ("C" "C++")))
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

;; Package dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Cathy.Chang's Emacs - For The Horder"
        dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-navigator t))



;; package rainbow-fart settings
(use-package rainbow-fart
  :ensure t
  :init (rainbow-fart-mode 1)
  :config
  (setq rainbow-fart-keyword-interval nil) ;play voice for every key word
  ;; (setq rainbow-fart-voice-model "AcFun")
  (setq rainbow-fart-voice-model "JustKowalski")
  )


(provide 'setup-misc)
;;; setup-misc.el ends here
