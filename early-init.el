;; Make startup faster by reducing the frequency of garbage
;; collection. The default is 800 kilobytes
(setq gc-cons-threshold most-positive-fixnum)

;; Increase the amount of data which Emacs reads from the process.
;; the default is 4 kilobytes make it 32 Megabyte
(setq read-process-output-max (* 1024 1024 32)) ; 32MB

(setq file-name-handler-alist nil)

;; In Emacs 27+, package initialization occurs before `user-init-file` is
;; loaded, but after `early-init-file`. Doom handles package initialization, so
;; we must prevent Emacs from doing it early
(advice-add #'package--ensure-init-file :override #'ignore)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


;; add the module path
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))
;; (add-to-list 'load-path "~/.emacs.d/custom")
;; (add-to-list 'load-path (concat user-emacs-directory "custom"))

;; Load private config files is exist.
;; you can override variable in file.
;; An example ~/.emacs.d/private.el
;; (let ((private-conf (expand-file-name "private.el" user-emacs-directory)))
;;   (when (file-exists-p private-conf)
;;     (load-file private-conf)))

