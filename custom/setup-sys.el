;;; package -- setup-sysfs
;;; Commentary:
;;; code:

(setq user-full-name "Wang Chang"
      user-mail-address "wangchang0528@gmail.com")

;; Package exec-path-from-shell
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))


;; Garbage Collector Magic Hack
(use-package gcmh
  :init
  (setq gcmh-idel-delay 5)
  (setq gcmh-high-cons-hreshold (* 64 1024 1024))
  (gcmh-mode 1)
  (gcmh-set-high-threshold))

;; encoding:
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

(global-so-long-mode)                   ;Handle long files
(global-hl-line-mode t)
(set-fringe-mode 10)

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

(setq initial-major-mode 'fundamental-mode)

;; Feature mode
(display-battery-mode t)
(column-number-mode t)
(size-indication-mode -1)
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(setq display-time-format "%m/%d[%u] %H:%M%p")

(toggle-frame-maximized)

(show-paren-mode t)
(setq-default show-paren-style 'expression)

;; highlight syntax
(global-font-lock-mode t)

(fset 'yes-or-no-p 'y-or-n-p)       ;using y-or-n instead yes-or-no
(setq delete-by-moving-to-trash t)      ;delete to trash

(setq
 confirm-kill-emacs 'y-or-n-p
 history-length                     1000
 ;; Disable non selected window highlight
 cursor-in-non-selected-windows     nil
 highlight-nonselected-windows      nil
 ;; PATH
 exec-path                          (append exec-path '("/usr/local/bin/"))
 ;; Backups disabled
 backup-inhibited                   t
 fringes-outside-margins            t
 select-enable-clipboard          t
 frame-resize-pixelwise t)

(setq-default tab-width 4)                       ; default to 4 visible spaces to display a tab

;; file Operations
(setq
 sentence-end-double-space nil
 make-backup-files nil
 indent-tabs-mode nil
 auto-save-default nil
 create-lockfiles nil
 global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
 mark-ring-max 5000                ; increase kill ring to contains 5000 entries
 mode-require-final-newline t      ; add a newline to end of file
 kill-whole-line t                 ;if NIL, kill whole line and move the next line up
 )

;;set the font
;; (set-frame-font "YaHei Consolas Hybrid-11")
;; (if (member "Fira Code" (font-family-list))
;; (set-frame-font "Fira Code-12"))
(set-face-attribute
 'default nil
 ;; :family "YaHei Consolas Hybrid"
 ;; :family "JetBrains Mono"
 ;; :family "MesloLGL Nerd Font Mono"
 ;; :family "Monaco"
 :family "Fira Code"
 :height 130
 :weight 'normal)

;; (font-family-list)


;; Save backup files in a dedicated directory
(use-package desktop
  :config
  (add-to-list 'desktop-globals-to-save 'register-alist)
  (setq desktop-lazy-verbose nil
        desktop-modes-not-to-save '(tags-table-mode emacs-lisp-mode)
        desktop-restore-eager 15)
  (desktop-save-mode 1))

;; history
(use-package saveplace
  :hook (after-init . save-place-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :diminish
  :hook (after-init . global-auto-revert-mode))



(add-hook 'sh-mode-hook (lambda ()
                          (setq tab-width 4)))

(delete-selection-mode)
;; show whitespace in diff-mode
(add-hook 'diff-mode-hook (lambda ()
                            (setq-local whitespace-style
                                        '(face
                                          tabs
                                          tab-mark
                                          spaces
                                          space-mark
                                          trailing
                                          indentation::space
                                          indentation::tab
                                          newline
                                          newline-mark))
                            (whitespace-mode 1)))


(provide 'setup-sys)
;;; setup-sys.el ends here
