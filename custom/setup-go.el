;;; Package -- Summary
;;; Commentary:
;;; Code:



;; Package go-guru
;; Integration of the Go 'guru' analysis tool into Emacs.
(use-package go-guru
  :ensure t
  :config
  (go-guru-hl-identifier-mode)
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode)
  )

;; Package company-go
;; company-mode backend for Go (using gocode)
(use-package company-go
  ;; :disabled t
  :ensure t
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-go))
  :after go-mode
  :bind (:map go-mode-map
              ;Godef jump key binding
              ("M-." . godef-jump)))

;; (defun setup-go-mode-compile()
;;   ;; Customize compile command to run go build
;;   (if (not (string-match "go" compile-command))
;;       (set (make-local-variable 'compile-command)
;;            "go build -v && go test -v && go vet")))

;;go-mode packages
;; REQUIREMENTS:
;; go get -u golang.org/x/tools/cmd/...
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/nsf/gocode
;; go get -u github.com/kisielk/errcheck
(use-package go-mode
  :ensure t
  :config
  ;; Use goimports instead of go-fmt
  (setq gofmt-command "goimports"
        go-fontify-function-calls nil
        company-idle-delay .1
        )
  ;; Call gofmt before saving
  (add-hook 'before-save-hook #'gofmt-before-save)
  ;;(add-hook 'go-mode-hook 'setup-go-mode-compile)
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'go-mode-hook
            (lambda()
              (set (make-local-variable 'company-backends) '(company-go))
              (company-mode)))
  :bind
  (:map go-mode-map
        ("M-." . go-guru-definition)
        ("C-c d" . godoc-at-point)
        ("C-c g" . godoc)
        ("C-c h" . go-guru-hl-identifier)
        )
  :mode "\\.go\\'"
  )


(provide 'setup-go)
;;; setup-go.el ends here
