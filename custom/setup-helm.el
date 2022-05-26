;;; Package -- helm stuff
;;; Commentary:
;;; code:

;; Package helm
;; Helm is an Emacs incremental and narrowing framework
(use-package helm
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  :init (progn
          (require 'helm-config)
          (let ((ad-redefinition-action 'accept))
            helm-mode 1))
  :bind (("M-x" . helm-M-x)
         ("C-c h c" . helm-occur)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)      ; *<major-mode> or /<dir> or !/<dir-not-desired> or @<regexp>
         :map helm-map
         ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
         ("C-i" . helm-execute-persistent-action) ; make TAB works in terminal
         ("C-z" . helm-select-action) ; list actions using C-z
         ;;     :map shell-mode-map
         ;;     ("C-c C-l" . helm-comint-input-ring) ; in shell mode
         :map minibuffer-local-map
         ("C-c C-l" . helm-minibuffer-history))
  :config (progn
            (helm-autoresize-mode 1)
            (setq helm-M-x-fuzzy-match t
                  helm-buffers-fuzzy-matching t
                  helm-recentf-fuzzy-match t
                  helm-semantic-fuzzy-match t
                  helm-imenu-fuzzy-match t
                  helm-locate-fuzzy-match t
                  helm-apropos-fuzzy-match t
                  helm-lisp-fuzzy-completion t
                  ;; open helm buffer inside current window, not occupy whole other window
                  helm-split-window-in-side-p t
                  ;; move to end or beginning of source when reaching top or bottom of source.
                  helm-move-to-line-cycle-in-source t
                  ;; search for library in `require' and `declare-function' sexp.
                  helm-ff-search-library-in-sexp t
                  ;; scroll 8 lines other window using M-<next>/M-<prior>
                          helm-scroll-amount 8
                  helm-ff-file-name-history-use-recentf t
                  helm-quick-update t
                  helm-autoresize-max-height 25
                  helm-autoresize-min-height 25)))

;; Package helm-descbinds
;; A convenient `describe-bindings' with `helm'
(use-package helm-descbinds
  :after (helm)
  :init
  (fset 'describe-bindings 'helm-descbinds)
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds))
  :config
  (helm-descbinds-mode)
  )

;; PACKAGE: helm-swoop
;; Efficiently hopping squeezed lines powered by helm interface
(use-package helm-swoop
  :bind (("C-c h o" . helm-swoop)
         ("C-c s" . helm-multi-swoop-all)
         :map helm-swoop-map
         ("M-i" . helm-multi-swoop-all-from-helm-swoop)
         )
  :config (setq helm-multi-swoop-edit-save t)
  )

;; Package helm-ag
;; the silver searcher with helm interface
(use-package helm-ag
  :after helm
  ;; :bind ("C-c a g" . helm-do-ag-project-root)
  )

(use-package helm-posframe
  :after helm
  :config (helm-posframe-enable)
  (setq helm-posframe-poshandler
        #'posframe-poshandler-frame-center)
  (setq helm-posframe-width 200)
  ;; (setq helm-posframe-height 300)
  (setq helm-posframe-parameters
        '((left-fringe . 10)
          (right-fringe . 10)))
  )

;; Package helm-gtags
;; GNU GLOBAL helm interface
(use-package helm-gtags
  :after helm
  :init
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  :config
  (setq helm-gtags-auto-update t
        helm-gtags-direct-helm-completing t
        helm-gtags-fuzzy-match t
        helm-gtags-ignore-case t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-suggested-key-mapping t
        )
  :bind (:map helm-gtags-mode-map
              ("C-c g a" . helm-gtags-tags-in-this-function)
              ("C-j" . helm-gtags-select)
              ("M-." . helm-gtags-dwim)
              ("M-," . helm-gtags-pop-stack)
              ("C-c <" . helm-gtags-previous-history)
              ("C-c >" . helm-gtags-next-history))
  )


(provide 'setup-helm)
;;; setup-helm.el ends here
