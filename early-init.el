;; Make startup faster by reducing the frequency of garbage
;; collection. The default is 800 kilobytes
(setq gc-cons-threshold most-positive-fixnum)

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

