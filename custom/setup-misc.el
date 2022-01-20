;;; Package -- misc stuff
;;; Commentary:
;;; Code:

;; Save backup files in a dedicated directory
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))
(desktop-save-mode 1)

;;linum-mode
(use-package linum
  :config (global-linum-mode 1)
  )

;; highlight-symbol
(use-package highlight-symbol
  :bind(("C-<f3>" . highlight-symbol)
        ("<f3>" . highlight-symbol-next)
        ("S-<f3>" . highlight-symbol-prev)
        ("M-<f3>" . highlight-symbol-query-replace)
        )
  )

;; package centaur-tabs
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-headline-match)
  (setq centaur-tabs-set-bar t
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker " ● "
        centaur-tabs-style "bar")
  (centaur-tabs-mode t)
  :bind
  ("C-9" . centaur-tabs-backward)
  ("C-0" . centaur-tabs-forward)
  )

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
  :init (load-theme 'doom-acario-dark t)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;; doom-modeline
(use-package doom-modeline
  :custom
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-buffer-file-name-style 'auto)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-buffer-encoding t)
  :config (doom-modeline-mode)
  )

(use-package all-the-icons
  :if (display-graphic-p)
  )

(use-package all-the-icons-dired
  :custom-face (all-the-icons-dired-dir-face ((t (:foreground nill))))
  :hook (dired-mode . all-the-icons-dired-mode)
  )

;; rainbow mode for display the color
(use-package rainbow-mode
  :config
  (progn
    (defun @-enable-rainbow ()
      (rainbow-mode t))
    (add-hook 'progn-mode-hook '@-enable-rainbow)))

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


(use-package youdao-dictionary
  :config
  (setq url-automatic-caching t)
  )
(global-set-key (kbd "C-c k") 'youdao-dictionary-search-at-point)
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

;; Package cmake-mode
;; major-mode for editing CMake sources
(use-package cmake-mode
  :mode ("CMakeLists.txt" "\\.cmake\\'")
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
