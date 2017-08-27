(setq inhibit-startup-message t)        ;disable startup message

(require 'package)                      ;package manager
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try                        ;let's you try packages without install them
  :ensure t)
(use-package which-key                  ;bring up help on key combinations
  :ensure t
  :config
  (which-key-mode))

(add-to-list 'load-path "~/.emacs.d/custom")
(require 'setup-sys)
(require 'setup-helm)
(require 'setup-helm-gtags)
;; (require 'setup-ggtags)
(require 'setup-editing)
(require 'setup-misc)
(require 'setup-python)
(require 'setup-programming)

