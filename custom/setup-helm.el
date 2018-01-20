;;; Package -- helm stuff
;;; Commentary:
;;; code:

(use-package helm
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  :demand t
  :bind (("M-x" . helm-M-x)
         ("C-c h c" . helm-occur)
         ("<f1> SPC" . helm-all-mark-rings) ; I modified the keybinding
         ("M-y" . helm-show-kill-ring)
         ("C-c h x" . helm-register)    ; C-x r SPC and C-x r j
         ("C-c h g" . helm-google-suggest)
         ("C-c h M-:" . helm-eval-expression-with-eldoc)
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
  :config
  (helm-autoresize-mode t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)
  (setq helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match    t)
  (setq helm-locate-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-lisp-fuzzy-completion t)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  (require 'helm-config)
  (setq helm-split-window-in-side-p         t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line t)
  (helm-mode 1)
  )
(use-package helm-descbinds
  :defer 5
  :init
  (fset 'describe-bindings 'helm-descbinds)
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds))
  )
;; use helm to list eshell history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

;;; save current position to mark ring
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

;; show minibuffer history with Helm
(define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
(define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

(define-key global-map [remap find-tag] 'helm-etags-select)
(define-key global-map [remap list-buffers] 'helm-buffers-list)

;; PACKAGE: helm-swoop
;; Locate the helm-swoop folder to your path
(use-package helm-swoop
  :ensure t
  :bind (("C-c h o" . helm-swoop)
         ("C-c s" . helm-multi-swoop-all)
         :map helm-swoop-map
         ("M-i" . helm-multi-swoop-all-from-helm-swoop)
         )
  :config (setq helm-multi-swoop-edit-save t)
  )
(use-package helm-ag
  :ensure t
  :after helm
  :bind ("C-c a g" . helm-do-ag-project-root))
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows t)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color t)

(provide 'setup-helm)
;;; setup-helm.el ends here
