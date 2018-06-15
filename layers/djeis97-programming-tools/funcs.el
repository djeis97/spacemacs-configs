(defun djeis97-programming-tools/company-tng-configure-default ()
  "Applies the default configuration to enable company-tng."
  (setq company-require-match nil)
  (setq company-frontends '(company-tng-frontend
                            company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))
  (let ((keymap company-active-map))
    (define-key keymap [return] nil)
    (define-key keymap (kbd "RET") nil)
    (define-key keymap [tab] 'company-complete-common)
    (define-key keymap (kbd "TAB") 'company-complete-common)))


