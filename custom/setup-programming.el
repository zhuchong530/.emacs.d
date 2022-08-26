;;; Package -- Programming stuff
;;; Commentary:
;;; Code:

(use-package cc-mode
  :mode (("\\.hpp\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         ("\\.m\\'" . c-mode)
         ("\\.mm\\'" . c-mode))

  :config
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
  (setq c-set-style "gnu") ;; set style to "k&r"
  (setq c-basic-offset 4)
  (setq tab-width 4) ; or any other preferred value
  )

;; Package function-args
;; C++ completion for GNU Emacs
(use-package function-args
  :disabled
  :config (fa-config-default)
  )

;; Package eldoc
(use-package eldoc
  :diminish eldoc-mode
  :config (add-hook 'prog-mode-hook 'eldoc-mode))

;; Package -flycheck
;; On-the-fly syntax checking
(use-package flycheck
  ;; :hook (lsp-mode . flycheck-mode)
  :diminish (flycheck-mode . "f")
  :init (setq-default flycheck-check-syntax-automatically '(save-mode-enabled))
  :bind
  (("C-c e n" . flycheck-next-error)
   ("C-c e p" . flycheck-previour-error)
   ("C-c e l" . flycheck-list-errors))
  :config
  (aset flycheck-error-list-format 5 '("Message" 0 t))
  )

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)
;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))



;; Package: projejctile
;; Manage and navigate projects in Emacs easily
(use-package projectile
  :requires (helm)
  :bind (:map projectile-mode-map
              ("C-c p" . 'projectile-command-map))
  :config
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-completion-system 'helm)
  (projectile-mode +1))

;;nasm-mode
(use-package nasm-mode
  :mode "\\.\\(nasm\\|s\\|asm\\)$"
  )

;;magit package
(use-package magit
  :commands magit-get-top-dir
  :bind (("C-x g s" . magit-status)
         ("C-x g f" . magit-log-buffer-file)
         ("C-x g x" . magit-checkout)
         ("C-x g c" . magit-commit)
         ("C-x g p" . magit-push)
         ("C-x g u" . magit-pull)
         ("C-x g e" . magit-ediff-resolve)
         ("C-x g r" . magit-rebase-interactive))
  :config (magit-auto-revert-mode)
  )

(use-package git-gutter+
  :config
  (progn
    (global-git-gutter+-mode)))

;; Package cmake-mode
;; major-mode for editing CMake sources
(use-package cmake-mode
  :mode ("CMakeLists.txt" "\\.cmake\\'")
  )

(use-package gud
  :commands gud-gdb
  :bind (("<f9>" . gud-cont)
         ("<f10>" . gud-next)
         ("<f11>" . gud-step)
         ("S-<f11>" . gud-finish))
  :init
  (defun show-debugger ()
    (interactive)
    (let ((gud-buf
           (catch 'found
             (dolist (buf (buffer-list))
               (if (string-match "\\*gud-" (buffer-name buf))
                   (throw 'found buf))))))
      if (gud-buf
          (switch-to-buffer-other-window gud-buf)
          (call-interactively 'gud-gdb))))
  )


(provide 'setup-programming)
;;; setup-programming.el ends here
