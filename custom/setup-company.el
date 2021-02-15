;;; Package -- Summary
;;; Commentary:
;;; Code:

(message "start loading setup-programming.el")

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
    '(add-to-list 'company-backends '(company-tabnine company-capf company-yasnippet company-abbrev company-dabbrev)))
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
  :hook (company-mode . company-box-mode)
  :config
  )

;; comapny-tabnine
;; OpenAI completion backend
(use-package company-tabnine
  :ensure t
  )
;; The free version of TabNine is good enough,
;; and below code is recommended that TabNine not always
;; prompt me to purchase a paid version in a large project.
(defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
  (let ((company-message-func (ad-get-arg 0)))
    (when (and company-message-func
               (stringp (funcall company-message-func)))
      (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
        ad-do-it))))



;; (message "after loading company-box")

;; Package compnay-lsp
;; add compnay-lsp as a backend
;; (use-package company-lsp
;;   :ensure t
;;   :config
;;   (push 'company-lsp company-backends)
;;   (setq company-lsp-cache-candidates nil
;;        company-lsp-enable-snippet t
;;        company-lsp-enable-recompletion t))

;; (message "after loading comapny-lsp")

(provide 'setup-company)
;;; setup-company.el ends here
