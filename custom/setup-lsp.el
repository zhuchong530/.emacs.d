;;; Package -- Summary
;;; Commentary:
;;; Code:

;; lsp-mode package
(use-package lsp-mode
  :bind (:map lsp-mode-map
              ("C-c f" . lsp-format-region)
              ("C-c a" . lsp-execute-code-action)
              ("C-c r" . lsp-rename)
              ("C-c e" . lsp-describe-thing-at-point)
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))
  :custom
  ;; debug 时开启log，否则影响性能
  (lsp-log-io nil)
  ;; 日志记录行数
  (lsp-log-max 1000)
  (lsp-keymap-prefix "C-l")
  (lsp-diagnostics-provider :flycheck)
  (lsp-diagnostics-flycheck-default-level 'warning)
  (lsp-completion-provider :none)       ; corfu.el: :none, company: :capf
  (lsp-enable-symbol-highlighting nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  ;; 启用snippet后才支持函数或方法的placeholder提示
  (lsp-enable-snippet nil)
  ;; 刷新高亮、lenses和links的间隔
  (lsp-idle-delay 0.2)
  (lsp-enable-folding nil)
  (lsp-enable-links nil)
  (lsp-enable-indentation nil)
  ;; flycheck会在modeline显示检查结果，故不需lsp再展示
  (lsp-modeline-diagnostics-enable nil)
  ;; 不在modeline上显示code-actions信息
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-restart 'auto-restart)
  ;; 使用projectile/project来自动探测项目根目录
  (lsp-auto-guess-root t)
  (lsp-imenu-sort-methods '(position))
  :init
  ;; 设置lsp使用corfu来进行补全
  (defun my/lsp-mode-setup-completion()
    (setf (alist-get 'styles
                     (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook(
        ;; To defer LSP server startup(and DidOen notifications) until the buffer is
        ;; visible, use `lsp-deferred` instead of `lsp`
        ;; (prog-mode-hook . lsp-deferred)
        (python-mode . lsp)
        (go-mode . lsp)
        (c-mode . lsp)
        (c++-mode . lsp)
        (lsp-mode . lsp-enable-which-key-integration))
  :config
  (dolist (dir '("[/\\\\][^/\\\\]*\\.\\(json\\|html\\|pyc\\|class\\|log\\|jade\\|md\\)\\'"
                 "[/\\\\]resources/META-INF\\'"
                 "[/\\\\]vendor\\'"
                 "[/\\\\]node_modules\\'"
                 "[/\\\\]\\.settings\\'"
                 "[/\\\\]\\.project\\'"
                 "[/\\\\]\\.travis\\'"
                 "[/\\\\]bazel-*"
                 "[/\\\\]\\.cache"
                 "[/\\\\]\\.clwb$"))
    (push dir lsp-file-watch-ignored-directories))
  )
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


(use-package consult-lsp
  :commands (consult-lsp-symbols consult-lsp-diagnostics consult-lsp-file-symbols)
  :config (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))


(provide 'setup-lsp)
;;; setup-lsp.el ends here
