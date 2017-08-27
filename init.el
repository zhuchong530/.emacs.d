;; main init file
;; Commentary:
;; Cathy.chang's GNU Emacs configuration
;;

;; code:



(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path (concat user-emacs-directory "custom"))
(require 'setup-sys)
(require 'setup-helm)
(require 'setup-helm-gtags)
(require 'setup-editing)
(require 'setup-misc)
(require 'setup-python)
(require 'setup-programming)

