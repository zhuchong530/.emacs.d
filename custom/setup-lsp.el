;;; Package -- Summary
;;; Commentary:
;;; Code:

;; lsp-mode package
(use-package lsp-mode
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point))
  :init
  (setq lsp-auto-guess-root nil           ;detect project root
        lsp-diagnostics-provider :flycheck
        lsp-log-io nil
        lsp-enable-indentation t
        lsp-enable-imenu t
        lsp-keymap-prefix "C-l"
        lsp-file-watch-threshold 500
        lsp-prefer-flymake nil)         ;use flycheck
  (defun lsp-on-save-operation()
    (when (or (boundp 'lsp-mode)
              (bound-p 'lsp-deferred))
      (lsp-organize-imports)
      (lsp-format-buffer)))
  :hook(
        ;; To defer LSP server startup(and DidOen notifications) until the buffer is
        ;; visible, use `lsp-deferred` instead of `lsp`
        ;; (prog-mode-hook . lsp-deferred)
        (python-mode . lsp-deferred)
        (go-mode . lsp-deferred)
        (c-mode . lsp-deferred)
        (c++-mode . lsp-deferred)
        (lsp-mode . lsp-enable-which-key-integration)))
;; Package lsp-ui
;; Optionally
(use-package lsp-ui
  :after (lsp-mode)
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :init (setq lsp-ui-doc-enable t
              lsp-ui-doc-use-webkit nil
              lsp-ui-doc-header nil
              lsp-ui-doc-delay 0.2
              lsp-ui-doc-include-signature t
              lsp-ui-doc-alignment 'at-point
              lsp-ui-doc-use-childframe nil
              lsp-ui-doc-border (face-background 'default)
              lsp-ui-peek-enable t
              lsp-ui-peek-show-directory t
              lsp-ui-sideline-update-mode 'line
              lsp-ui-sideline-enable t
              lsp-ui-sideline-show-code-actions t
              lsp-ui-sideline-show-hover nil
              lsp-ui-sideline-ignore-duplicate t)
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))
  ;; "C-g" to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
  ;; Reset `lsp-ui-dock-background` after loading theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setq lsp-ui-doc-border (face-background 'default))
              (set-face-background 'lsp-ui-dock-background
                                   (face-background 'tooltip))))
  )


(provide 'setup-lsp)
;;; setup-lsp.el ends here
