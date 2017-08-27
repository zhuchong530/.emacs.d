;; sys
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq gc-cons-threshold 100000000)     ;garbage collect threshold
(defalias 'yes-or-no-p 'y-or-n-p)       ;using y-or-n instead yes-or-no
(setq make-backup-files nil)
(setq column-number-mode t)
;; exec-path-from-shell packages
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))
(setenv "GOPATH" "/home/wangchang/data/Programme/googleGo/")
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


(toggle-frame-maximized)
;; Shift + up/down/left/right  to move among window
(windmove-default-keybindings)


(provide 'setup-sys)
