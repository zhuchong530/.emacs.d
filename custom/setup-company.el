;;; Package -- Summary
;;; Commentary:
;;; Code:

;; company
;; Modular text completion framework
(use-package company
  :ensure t
  :defer 5
  :commands (company-mode company-complete company-complete-common company-complete-selection helm-company)
  :hook (after-init . global-company-mode)
  :init
  (setq company-minimum-prefix-length 1
        company-require-match nil
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-limit 30        ;bigger popup window
        company-tooltip-minimum-width 40
        company-tooltip-align-annotations t ;align annotations to the right tooltip border
        company-eclim-auto-save nil)        ;ends setq
  (eval-after-load 'company
    ;;'(add-to-list 'company-backends '(company-tabnine company-capf company-yasnippet company-abbrev company-dabbrev)))
    ;; '(add-to-list 'company-backends '(company-tabnine company-capf company-yasnippet)))
    '(add-to-list 'company-backends '(company-capf company-yasnippet)))
  :config
  ;; dropdown by default=0, no dropdown=1
  (setq company-idle-delay 0)
  (defun jcs--company-complete-selection--advice-around(fn)
    "Advice execute around `company-complete-selection` command"
    (let ((company-dabbrev-downcase t))
      (call-interactively fn)))
  (advice-add 'company-complete-selection :around#'jcs--company-complete-selection--advice-around)
  (defun complete-or-indent ()
    (interactive)
    (if (company-manual-begin)
        (company-complete-common)
      (indent-according-to-mode)))
  :bind (
         ("C-."   . company-complete)
         (:map company-active-map
               ("M-n"    . company-select-next)
               ("M-p"    . company-select-previous)
               ([return] . company-complete-selection)
               ("C-w"    . backward-kill-word)
               ("C-g"    . company-abort)
               ("C-c"    . company-search-abort)
               ("<tab>"  . complete-or-indent)
               ("C-s"  . company-search-candidates)
               ("C-o" . company-search-toggle-filtering)
               ));end bind
  );end use-package company
;; (message "after loading company")

;; Package company-box
;; A company front-end - Differences with the built-in front-end
(use-package company-box
  :after (all-the-icons)
  :diminish
  :hook (company-mode . company-box-mode)
  :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (setq company-box-backends-colors nil)
  (setq company-box-show-single-candidate t)
  (setq company-box-max-candidates 50)
  (with-eval-after-load 'all-the-icons
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (setq company-box-icons-all-the-icons
           `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.2))
          (Text . ,(all-the-icons-faicon "text-width" :height 0.85 :v-adjust -0.05))
          (Method . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Function . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Constructor . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Field . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
          (Variable . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
          (Class . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Interface . ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Module . ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Property . ,(all-the-icons-faicon "wrench" :height 0.85 :v-adjust -0.05))
          (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.9 :v-adjust -0.2))
          (Value . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Enum . ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.9 :v-adjust -0.2))
          (Snippet . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))
          (Color . ,(all-the-icons-material "palette" :height 0.9 :v-adjust -0.2))
          (File . ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.05))
          (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.9 :v-adjust -0.2))
          (Folder . ,(all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05))
          (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Constant . ,(all-the-icons-faicon "square-o" :height 0.9 :v-adjust -0.05))
          (Struct . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Event . ,(all-the-icons-faicon "bolt" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-orange))
          (Operator . ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.2))
          (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
          (Template . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2)))))
  )

;; ;; comapny-tabnine
;; ;; OpenAI completion backend
;; (use-package company-tabnine
;;   :ensure t
;;   )
;; ;; The free version of TabNine is good enough,
;; ;; and below code is recommended that TabNine not always
;; ;; prompt me to purchase a paid version in a large project.
;; (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
;;   (let ((company-message-func (ad-get-arg 0)))
;;     (when (and company-message-func
;;                (stringp (funcall company-message-func)))
;;       (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
;;         ad-do-it))))


(provide 'setup-company)
;;; setup-company.el ends here
