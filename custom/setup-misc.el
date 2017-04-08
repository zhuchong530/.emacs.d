;; GROUP: misc

;; Remove all backup files
;; (setq make-backup-files nil)
;; (setq backup-inhibited t)
;; (setq auto-save-default nil)

;; Save backup files in a dedicated directory
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))

;; exec-path-from-shell package
;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))

(desktop-save-mode 1)

;;linum-mode
(require 'linum)
(global-linum-mode 1)

;; highlight-symbol
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;;package tabbar
(require 'tabbar)
(tabbar-mode t)
(global-set-key (kbd "C-M-9") 'tabbar-backward-group)
(global-set-key (kbd "C-M-0") 'tabbar-forward-group)
(global-set-key (kbd "C-9") 'tabbar-backward)
(global-set-key (kbd "C-0") 'tabbar-forward)
;; 设置tabbar外观
;; 设置默认主题: 字体, 背景和前景颜色，大小
(set-face-attribute 'tabbar-default nil
                    ;:family "DejaVu Sans Mono"
                   :background "gray80"
                   :foreground "gray30"
                   :height 1.0
                    )
;; 设置左边按钮外观：外框框边大小和颜色
(set-face-attribute 'tabbar-button nil
                    :inherit 'tabbar-default
                    :box '(:line-width 1 :color "yellow")
                    )
;; 设置当前tab外观：颜色，字体，外框大小和颜色
(set-face-attribute 'tabbar-selected nil
                    :inherit 'tabbar-default
                    :foreground "DarkGreen"
                    :background "LightGoldenrod"
                   :box '(:line-width 2 :color "DarkGoldenrod")
                    :overline "black"
                    :underline "black"
                    :weight 'bold
                   )
;; 设置非当前tab外观：外框大小和颜色
(set-face-attribute 'tabbar-unselected nil
                    :inherit 'tabbar-default
                   :box '(:line-width 2 :color "#00B2BF")
                   )

;;package ace-jump-mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;package golden-ratio
(require 'golden-ratio)
(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)
(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))
;; do not enable golden-raio in thses modes
(setq golden-ratio-exclude-modes '("ediff-mode"
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
                                   ;; "magit-log-mode"
                                   ;; "magit-reflog-mode"
                                   ;; "magit-status-mode"
                                   "IELM"
                                   "eshell-mode" "dired-mode"))
(golden-ratio-mode)

;;color-theme
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-dark-laptop)

;;smart-mode-line
(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/setup)
;(set sml/theme 'light)
(set sml/theme 'dark)
;;(set sml/theme 'respectful)

;; Package - Dash-at-point
;; search documents in Dash API docsets
;; (add-to-list 'load-path "/path/to/dash-at-point")
;; (autoload 'dash-at-point "dash-at-point"
;;   "Search the word at point with Dash." t nil)
;; (global-set-key "\C-cd" 'dash-at-point)
;; (global-set-key "\C-ce" 'dash-at-point-with-docset)



(toggle-frame-maximized)

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
(require 'tramp)

(provide 'setup-misc)
