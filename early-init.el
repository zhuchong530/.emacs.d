;;; package -- Summary
;;; Commentary:
;; early-init.el --- Early Init File
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:



;; Garbage Collection
;; A big contributer to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later.
(setq gc-cons-percentage-original gc-cons-percentage
      gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)

;; Resizing the Emacs frame can be terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Increase the amount of data which Emacs reads from the process.
;; the default is 4 kilobytes make it 32 Megabyte
(setq read-process-output-max (* 1024 1024 32)) ; 32MB

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist)
(push '(underorated . t) default-frame-alist)


;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)

;; native-comp settings
(when (and (featurep 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache (expand-file-name "eln-cache/" user-emacs-directory))
  (setq-default native-comp-speed 2
               native-comp-async-query-on-exit t
               native-comp-jit-compilation nil
               native-comp-async-report-warnings-errors 'silent))

;; Prevent unwanted runtime builds in gcemacs (native-comp); packages are
;; compiled ahead-of-time when they are installed and site files are compiled
;; when gccemacs is installed
;; (setq comp-deferred-compilation nil)

;; 在单独文件保存自定义配置，避免污染~/.emacs文件
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(add-hook 'after-init-hook (lambda () (when (file-exists-p custom-file) (load custom-file))))

;; Load private config files is exist.
;; you can override variable in file.
;; An example ~/.emacs.d/private.el
;; (let ((private-conf (expand-file-name "private.el" user-emacs-directory)))
;;   (when (file-exists-p private-conf)
;;     (load-file private-conf)))

(provide 'early-init)

;;; early-init.el ends here
