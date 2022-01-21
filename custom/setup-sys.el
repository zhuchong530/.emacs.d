;;; package -- setup-sysfs
;;; Commentary:
;;; code:

(use-package dash)
(use-package diminish)

;; Package exec-path-from-shell
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;; encoding:
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; (setenv "LANG" "en_US.UTF-8")
;; (setenv "LC_ALL" "en_US.UTF-8")
;; (setenv "LC_CTYPE" "en_US.UTF-8")

;; Feature mode
(display-time-mode 1)
(column-number-mode 1)
(show-paren-mode t)
(setq-default show-paren-style 'expression)
(display-battery-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(global-auto-revert-mode t)

(fset 'yes-or-no-p 'y-or-n-p)       ;using y-or-n instead yes-or-no
(toggle-frame-maximized)
(setq gc-cons-threshold (* 8192 8192))     ;garbage collect threshold
(setq read-process-output-max (* 1024 1024 128)) ; 128MB
(setq inhibit-startup-message t)        ;disable startup message
(setq delete-by-moving-to-trash t)      ;delete to trash

(setq
 confirm-kill-emacs 'y-or-n-p
 ;; Disable backups (that's what git/dropbox are for)
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

;; file Operations
(setq
 tab-width 4
 inhibit-splash-screen t
 initial-scratch-message nil
 sentence-end-double-space nil
 make-backup-files nil
 indent-tabs-mode nil
 auto-save-default nil
 create-lockfiles nil)

;;set the font
(set-frame-font "YaHei Consolas Hybrid-11")
;; (set-frame-font "Azeret Mono Light-11")
 ;; (if (member "Consolas" (font-family-list))
     ;; (set-frame-font "Monaco-12"))

;; History
(savehist-mode 1)
(setq savehist-file "~/.emacs.d/savehist")
(setq history-length t
      history-delete-duplicates t)

(require 'bind-key)

(use-package which-key                  ;bring up help on key combinations
  :defer 10
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;; package ace-window
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-c p") 'ace-window))

;; symon settings
(use-package symon
  :init (symon-mode)
  )


(provide 'setup-sys)
;;; setup-sys.el ends here
