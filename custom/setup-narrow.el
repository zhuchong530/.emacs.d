;;; Package -- narrowing stuff
;;; Commentary:
;;; code:



;; Orderless
;; Controls the sorting of the minibuffer completions
(use-package orderless
  :custom ((completion--styles '(orderless))
           (completion-category-defaults nil)
           (completion--category-override '((file (style . (partial-completion)))))))

(use-package selectrum
  :bind (("C-M-r" . selectrum-repeat)
         :map selectrum-minibuffer-map
         ("C-r" . selectrum-select-from-history)
         ("C-n" . selectrum-next-candidate)
         ("C-p" . selectrum-previous-candidate)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :custom
  (selectrum-fix-minibuffer-height t)
  (selectrum-num-candidates-displayed 9)
  (selectrum-refine-candidates-function #'orderless-filter)
  (selectrum-highlight-candidates-function #'orderless-highlight-matches)
  :custom-face
  (selectrum-current-candidate ((t (:background "#3a3f5a"))))
  :init (selectrum-mode 1))

(use-package consult
  :bind (("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ;; ("C-c k" . consult-kmacro)
         ("C-x M-:" . consult-complex-command) ;orig: repeat-complex-command
         ("C-x b" . consult-buffer)     ; orig: switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;orig: switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custome M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;orgin: abbrev-prefix-mark
         ("C-M-#" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)    ; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)  ; Orig: goto-line
         ("M-g M-g" . consult-goto-line) ; Orig: goto-line
         ("M-g o" . consult-outline)     ;Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occurg)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history) ; orig: isearch-edit-string
         ("M-s e" . consult-isearch-history) ; orig: isearch-edit-string
         ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)      ; orig: next-matching-history-element
         ("M-r" . consult-history))      ; orig: previous-matching-history-element
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for consult-register, consult-register-load, consult-register-store
  ;; and the Emacs built-ins
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Use consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the consult-customize macro
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))
  (setq consult-narrow-key "<")
  )



;; Marginalia
;; Enhances the minibuffer completions with additional informations
(use-package marginalia
  :custom (marginalia-annotators
           '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))



(provide 'setup-narrow)
;;; setup-helm.el ends here
