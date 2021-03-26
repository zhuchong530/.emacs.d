;;; Package -- editor stuff
;;; Commentary:
;;; code:

(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      tab-width 4                       ; default to 4 visible spaces to display a tab
      kill-whole-line t                 ;if NIL, kill whole line and move the next line up
      )

(add-hook 'sh-mode-hook (lambda ()
                          (setq tab-width 4)))

(delete-selection-mode)
(global-set-key (kbd "RET") 'newline-and-indent)
;; show whitespace in diff-mode
(add-hook 'diff-mode-hook (lambda ()
                            (setq-local whitespace-style
                                        '(face
                                          tabs
                                          tab-mark
                                          spaces
                                          space-mark
                                          trailing
                                          indentation::space
                                          indentation::tab
                                          newline
                                          newline-mark))
                            (whitespace-mode 1)))

;; Package: volatile-highlights
;; Minor mode for visual feedback on some operations.
;; An example is that if you paste (yank) a block of text,
;; it will be highlighted until you press the next key.
;; This is just a small tweak, but gives a nice bit of visual feedback.
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :commands (volatile-highlights-mode)
  :init
  (add-hook 'after-init-hook '(lambda() (volatile-highlights-mode t)))
  :config
  (set-face-attribute 'vhl/default-face nil
                      :underline "light slate gray"))

;; Package: clean-aindent-mode
;; Simple indent and unindent, trims indent white-space
(use-package clean-aindent-mode
  :ensure t
  :commands (clean-aindent-mode)
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode)
  )

;; PACKAGE: dtrt-indent
;; Adapt to foreign indentation offsets
(use-package dtrt-indent
  :ensure t
  :config
  (setq global-mode-string (remove 'dtrt-indent-mode-line-info global-mode-string))
  (dtrt-indent-mode 1)
  )

;; PACKAGE: ws-butler
;; Unobtrusively remove trailing whitespace.
(use-package ws-butler
  :ensure t
  :diminish
  :hook (prog-mode . ws-butler-mode))

;; Package: undo-tree
;; Treat undo history as a tree
(use-package undo-tree
  :ensure t
  :init (progn
          (global-undo-tree-mode)
          (setq undo-tree-visualizer-timestamps t)
          (setq undo-tree-visualizer-diff t))
  )

;; uniquify
(use-package uniquify
  :ensure nil
  :config
  (setq
   uniquify-buffer-name-style 'reverse
   uniquify-separator ":"
   uniquify-after-kill-buffer-p t
   uniquify-ignore-buffers-re "^\\*"
   )
  )

;; PACKAGE: smartparens
;; Automatic insertion, wrapping and paredit-like navigation with user defined pairs.
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode 1)
  (smartparens-global-mode 1)
  (sp-with-modes '(c-mode c++-mode javascript-mode go-mode python-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                              ("* ||\n[i]" "RET"))))
  )

;; PACKAGE: comment-dwim-2
;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :ensure t
  :bind ("M-;" . comment-dwim-2)
  )

;; PACKAGE: anzu
;; Show number of matches in mode-line while searching
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :commands (global-anzu-mode)
  :init
  (add-hook 'after-init-hook '(lambda() (global-anzu-mode +1)))
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace))
  )

;; PACKAGE: iedit
;; Edit multiple regions in the same way simultaneously.
(use-package iedit
  :ensure t
  :init (setq iedit-toggle-key-default nil)
  :bind ("C-;" . iedit-mode)
  )

;; package indent-guide
(use-package indent-guide
  :ensure t
  :hook (after-init-hook . indent-guide-global-mode))


(provide 'setup-editing)
;;; setup-editing.el ends here
