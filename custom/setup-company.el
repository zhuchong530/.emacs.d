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
  :hook (company-mode . company-box-mode)
  :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-backends-colors nil
        ;; These are the Doom Emacs Defaults
        company-box-icons-all-the-icons
        '((Unknow . ,(all-the-icons-material "find-in_page" :face 'all-the-icons-purple))
          (Text . ,(all-the-icons-material "text-fields" :face 'all-the-icons-green))
          (Method . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
          (Function . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
          (Constructor . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
          (Field . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
          (Variable . ,(all-the-icons-material "adjust" :face 'all-the-icons-blue))
          (Class . ,(all-the-icons-material "class" :face 'all-the-icons-red))
          (Interface . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
          (Module . ,(all-the-icons-material "view_module" :face 'all-the-icons-red))
          (Property . ,(all-the-icons-material "settings" :face 'all-the-icons-red))
          (Unit . ,(all-the-icons-material "straighten" :face 'all-the-icons-red))
          (Value . ,(all-the-icons-material "filter_1" :face 'all-the-icons-red))
          (Enum . ,(all-the-icons-material "plus_one" :face 'all-the-icons-red))
          (Keyword . ,(all-the-icons-material "filter_center_focus" :face 'all-the-icons-red))
          (Snippet . ,(all-the-icons-material "short_text" :face 'all-the-icons-red))
          (Color . ,(all-the-icons-material "color_lens" :face 'all-the-icons-red))
          (File . ,(all-the-icons-material "insert_drive_file" :face 'all-the-icons-red))
          (Reference . ,(all-the-icons-material "collections_bookmark" :face 'all-the-icons-red))
          (Folder . ,(all-the-icons-material "folder" :face 'all-the-icons-red))
          (EnumMember . ,(all-the-icons-material "people" :face 'all-the-icons-red))
          (Constant . ,(all-the-icons-material "pause_circle_filled" :face 'all-the-icons-red))
          (Struct . ,(all-the-icons-material "streetview" :face 'all-the-icons-red))
          (Event . ,(all-the-icons-material "event" :face 'all-the-icons-red))
          (Operator . ,(all-the-icons-material "control_point" :face 'all-the-icons-red))
          (TypeParameter . ,(all-the-icons-material "class" :face 'all-the-icons-red))
          (Template . ,(all-the-icons-material "short_text" :face 'all-the-icons-green))))
  ;; Add a space after the icon
  ;; (dolist (elt company-box-icons-all-the-icons)
  ;;   (setcdr elt (concat (cdr elt) " ")))
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
