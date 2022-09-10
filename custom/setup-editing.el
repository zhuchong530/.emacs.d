;;; Package -- editor stuff
;;; Commentary:
;;; code:


(use-package which-key                  ;bring up help on key combinations
  :diminish
  :hook (after-init . which-key-mode)
  :config
  (which-key-setup-side-window-bottom))

;; package ace-window
(use-package ace-window
  :config
  (global-set-key (kbd "C-c o") 'ace-window)
  ;; 设置为frame后会忽略treemacs frame，否则即使两个窗口时也会提示选择
  (setq aw-scope 'frame)
  ;; 调大窗口选择字符
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :foreground "red" :height 5.0)))))
  )

;; Package: volatile-highlights
;; Minor mode for visual feedback on some operations.
;; An example is that if you paste (yank) a block of text,
;; it will be highlighted until you press the next key.
;; This is just a small tweak, but gives a nice bit of visual feedback.
(use-package volatile-highlights
  :commands (volatile-highlights-mode)
  :init
  (add-hook 'after-init-hook 'volatile-highlights-mode)
  :config
  (set-face-attribute 'vhl/default-face nil
                      :underline "light slate gray"))

;; Package: clean-aindent-mode
;; Simple indent and unindent, trims indent white-space
(use-package clean-aindent-mode
  :commands (clean-aindent-mode)
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode)
  )

;; PACKAGE: ws-butler
;; Unobtrusively remove trailing whitespace.
(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;; Package: undo-tree
;; Treat undo history as a tree
(use-package undo-tree
  :init (progn
          (global-undo-tree-mode)
          (setq undo-tree-visualizer-timestamps t)
          (setq undo-tree-auto-save-history nil)
          (setq undo-tree-visualizer-diff t))
  )

;; uniquify
(use-package uniquify
  :ensure nil
  :config
  (setq
   uniquify-buffer-name-style 'forward
   uniquify-separator ":"
   uniquify-after-kill-buffer-p t
   uniquify-ignore-buffers-re "^\\*"))

;; auto close bracket insertion. New in emacs 24
;; built-in electric-pair
(electric-pair-mode 1)
;; make electric-pair-mode work on more brackets
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\' . ?\')
        (?\[ . ?\])
        (?\` . ?\`)
        (?\{ . ?\})))

;; PACKAGE: comment-dwim-2
;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2)
  )

;; PACKAGE: iedit
;; Edit multiple regions in the same way simultaneously.
(use-package iedit
  :init (setq iedit-toggle-key-default nil)
  :bind ("C-;" . iedit-mode)
  )

;; package indent-guide
(use-package indent-guide
  :hook (after-init-hook . indent-guide-global-mode))


(provide 'setup-editing)
;;; setup-editing.el ends here
