(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)
;; replace the `completion-at-point' and `completion-symbol' bindings in
;; irony-mode's buffers by irony-modes function
(defun my-irony-mode-hook()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; highlight-symbol
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;;nasm-mode
(require 'nasm-mode)
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))
;; disaster
;;(require 'disaster)
;;(define-key c-mode-base-map (kbd "C-c d") 'disaster)

;;;;;;;;;;;;;;;;;;;;;;;; golang settings
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
;;go-mode packages
(require 'go-mode)
;removes all unused imports
(add-hook 'go-mode-hook '(lambda() (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
;; format the current buffer
(add-hook 'go-mode-hook '(lambda() (local-set-key (kbd "C-c C-f") 'gofmt)))
;; format the buffer when save
(add-hook 'before-save-hook 'gofmt-before-save)
;; show the go documentation for a given package
(add-hook 'go-mode-hook '(lambda() (local-set-key (kbd "C-c C-k") 'godoc)))

;; gocode autocomplete
(add-hook 'go-mode-hook '(lambda()
                           (set (make-local-variable 'company-backends) '(company-go))
                           (company-mode)))
;; go-eldoc packages
(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(set-face-attribute 'eldoc-highlight-function-argument nil
                    :underline t :foreground "green"
                    :weight 'bold)
;; go-guru packages
(require 'go-guru)
(go-guru-hl-identifier-mode)
(add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)

;;magit package
(require 'magit)



(provide 'setup-programming)
