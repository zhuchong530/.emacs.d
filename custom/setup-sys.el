;;; package -- setup-sysfs
;;; Commentary:
;;; code:

(eval-and-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; bug-hunter settings
(use-package bug-hunter
  :ensure t
  :defer t
  )

(use-package try                        ;let's you try packages without install them
  :ensure t)
(use-package which-key                  ;bring up help on key combinations
  :ensure t
  :defer 10
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode))

;; UTF-8
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(defalias 'yes-or-no-p 'y-or-n-p)       ;using y-or-n instead yes-or-no
(global-auto-revert-mode t)
(setq gc-cons-threshold 100000000)     ;garbage collect threshold
(setq inhibit-startup-message t)        ;disable startup message

(setq
 confirm-kill-emacs 'y-or-n-p
 confirm-nonexistent-file-or-buffer  t
 save-interprogram-paste-before-kill t
 mouse-yank-at-point                 t
 require-final-newline              nil
 visible-bell                       nil
 ring-bell-function                 'ignore
 ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
 minibuffer-prompt-properties
 '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 ;; persistent bookmarks
 ;; Disable backups (that's what git/dropbox are for)
 history-length                     1000
 auto-save-default                  nil
 make-backup-files                  nil
 create-lockfiles                   nil
 ;; Disable non selected window highlight
 cursor-in-non-selected-windows     nil
 highlight-nonselected-windows      nil
 ;; PATH
 exec-path                          (append exec-path '("/usr/local/bin/"))
 ;; Backups disabled
 backup-inhibited                   t
 make-backup-files                  nil
 indent-tabs-mode                   nil
 inhibit-startup-message            t
 fringes-outside-margins            t
 x-select-enable-clipboard          t
 use-package-always-ensure          t
 vc-follow-symlinks                 t
 auto-revert-check-vc-info          nil
 frame-resize-pixelwise t)

(setq column-number-mode t)
(setenv "GOPATH" "~/Programme/googleGo/")

;;set the font
(if (member "Monaco" (font-family-list))
    (set-frame-font "Monaco-14"))

;; window-system settings
(if window-system
    (progn
      (setq frame-title-format '(buffer-file-name "%f" ("%b")))
      (tooltip-mode -1)
      (tool-bar-mode -1)
      (mouse-wheel-mode t)
      (scroll-bar-mode -1))
  (display-time-mode 1)
  (menu-bar-mode -1))

(show-paren-mode t)
(setq-default show-paren-style 'expression)

(toggle-frame-maximized)
;; Shift + up/down/left/right  to move among window
(windmove-default-keybindings)

;; symon settings
(use-package symon
  :ensure t
  :init (symon-mode)
  )

(provide 'setup-sys)
;;; setup-sys.el ends here
