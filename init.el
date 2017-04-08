(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(defconst demo-packages
  '(anzu                                ; Show number of matches in mode-line while searching
    clean-aindent-mode                  ; Simple indent and unindent, trims indent white-space
    comment-dwim-2                      ; An all-in-one comment command to rule them all
    seq
    let-alist
    company
    company-c-headers                   ; Company mode backend for C/C++ header files
    duplicate-thing
    iedit
    yasnippet
    smartparens
    projectile
    volatile-highlights
    undo-tree
    multi-web-mode
    python-mode
    rainbow-mode
    js2-mode
    ace-jump-mode                       ;a quick cursor location minor mode for emacs
    golden-ratio
    switch-window
    zencoding-mode
    color-theme                         ;install color themes
    tabbar
    irony
    irony-eldoc
    company-irony                       ; company-mode completion back-end for irony-mode
    company-irony-c-headers             ; Company mode backend for C/C++ header files with Irony
    flycheck
    flycheck-irony
    smart-mode-line
    dtrt-indent                         ; Adapt to foreign indentation offsets
    go-mode
    go-guru
    go-eldoc
    nasm-mode                           ;disassembly mode
    disaster                            ;disassemble a c/c++ file
    zygospore))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package demo-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'setup-helm)
(require 'setup-helm-gtags)
;; (require 'setup-ggtags)
(require 'setup-editing)
(require 'setup-misc)
(require 'setup-python)
(require 'setup-programming)


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

;; Shift + up/down/left/right  to move among window
(windmove-default-keybindings)

;; function-args
(require 'function-args)
(fa-config-default)
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;;(delete 'company-semantic company-backends)
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)
;; (define-key c-mode-map  [(control tab)] 'company-complete)
;; (define-key c++-mode-map  [(control tab)] 'company-complete)

;; Package - company-irony
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
;; Package - flycheck-irony
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-include-path
                           (list (expand-file-name "/usr/include")))))
;; (add-hook 'c-mode-hook
;;           (lambda () (setq flycheck-clang-include-path
;;                            (list (expand-file-name "~/Programme/tlpi/execise/lib")))))
;; Package - irony-eldoc
(add-hook 'irony-mode-hook 'irony-eldoc)
;; Package - company-irony-c-headers
(require 'company-irony-c-headers)
;; Load with `irony-mode` as a grouped backend
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

;; company-c-headers
(add-to-list 'company-backends 'company-c-headers)

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq
 c-default-style "k&r" ;; set style to "linux"
 )
(setq c-basic-offset 4)

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; Package: projejctile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)

;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

(set-frame-font "Monaco-14")
;;(set-frame-font "Mononoki-14")
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")
(setq make-backup-files nil)
(tool-bar-mode 0)
(setq column-number-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(delete-selection-mode nil)
 '(package-selected-packages
   (quote
    (irony company-go go-eldoc go-guru exec-path-from-shell go-mode disaster jedi highlight-symbol nasm-mode zygospore zencoding-mode yasnippet ws-butler volatile-highlights undo-tree tabbar switch-window smartparens smart-mode-line rainbow-mode python-mode powerline multi-web-mode js2-mode irony-eldoc iedit helm-swoop helm-projectile helm-gtags golden-ratio ggtags function-args flycheck-irony duplicate-thing dtrt-indent company-irony-c-headers company-irony company-c-headers comment-dwim-2 color-theme clean-aindent-mode anzu ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
