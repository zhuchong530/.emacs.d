;;; Package -- osx special settings
;;; Commentary:
;;; Code:

;; On OS X Emacs doesn't use the shell Path if it's not started from
;; the shell. Let's fix that.
(use-package exec-path-from-shell
  :defer t
  :config
  (exec-path-from-shell-initialize)
  )

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta))

(provide 'setup-osx)
;;; setup-osx.el ends here
