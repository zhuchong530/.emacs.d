;;; Package -- main init file
;;; Commentary:
;;; My GNU Emacs configuration
;;; code:

;; emacs version must >= 26
(when (version< emacs-version "26")
  (error "Requires at least GNU Emacs 26, but you're running %s" emacs-version))

;package manager
(require 'package)
(setq package-archives '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ;; ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
(package-initialize)

;; bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish)
  (package-install 'bind-key))

;; load use-package, used for loading packages everywhere else
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(setq use-package-verbose t)
(setq use-package-always-ensure t)
;; (setq debug-on-error t)

(require 'setup-sys)
(require 'setup-editing)
(require 'setup-deffunc)
(require 'setup-misc)
(require 'setup-helm)
(require 'setup-company)
(require 'setup-programming)
(require 'setup-python)
(require 'setup-go)
(require 'setup-lsp)
(require 'setup-web)
(require 'setup-rust)
(require 'setup-org)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("266ecb1511fa3513ed7992e6cd461756a895dcc5fef2d378f165fed1c894a78c" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "76ed126dd3c3b653601ec8447f28d8e71a59be07d010cd96c55794c3008df4d7" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "da53441eb1a2a6c50217ee685a850c259e9974a8fa60e899d393040b4b8cc922" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "f8cf128fa0ef7e61b5546d12bb8ea1584c80ac313db38867b6e774d1d38c73db" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" "8e959d5a6771b4d1e2177263e1c1e62c62c0f848b265e9db46f18754ea1c1998" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "065da018725a9c72ee0358cb8e52bedfd402b222b971ba39b21a17bba84b0626" "4aafea32abe07a9658d20aadcae066e9c7a53f8e3dfbd18d8fa0b26c24f9082c" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "a3b6a3708c6692674196266aad1cb19188a6da7b4f961e1369a68f06577afa16" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "2eb22fdddfd785dff3d1a6a851314cbaea36b128ff336e60ba73cf0d3aaa6b1f" default))
 '(delete-selection-mode nil)
 '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
 '(global-undo-tree-mode t)
 '(indent-guide-global-mode t)
 '(org-agenda-files
   '("~/Google Driver/All Notes/Agenda/study.org" "~/Google Driver/All Notes/Agenda/work.org" "~/Google Driver/All Notes/Agenda/life.org"))
 '(package-selected-packages
   '(vertico corfu orderless marginalia org-roam org-roam-ui nyan-mode gcmh all-the-icons-dired rainbow-fart centaur-tabs flycheck-rust cargo rustic youdao-dictionary ace-window yasnippet-snippets indent-guide all-the-icons company-tabnine go-eldoc rainbow-delimiters go-mode bug-hunter undo-tree lsp-python-ms helm-lsp lsp-mode lsp-ui doom-modeline zeal-at-point cmake-ide cmake-mode diminish web-mode flycheck-go projejctile ws-butler which-key volatile-highlights use-package switch-window smartparens rainbow-mode python-mode powerline nasm-mode multi-web-mode magit js2-mode iedit highlight-symbol helm-swoop helm-projectile helm-gtags helm-descbinds helm-ag golden-ratio function-args exec-path-from-shell duplicate-thing comment-dwim-2 color-theme clean-aindent-mode anzu ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :foreground "red" :height 5.0)))))


;;; init.el ends here
