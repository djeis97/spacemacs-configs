
(define-minor-mode paren-management
  "Bindings to better manage parens."
  :keymap (let ((map (make-sparse-keymap)))
            (when (configuration-layer/package-usedp 'adjust-parens)
              (define-key map (kbd "<C-tab>") 'lisp-indent-adjust-parens)
              (define-key map (kbd "<C-iso-lefttab>") 'lisp-dedent-adjust-parens))
            (when (configuration-layer/package-usedp 'smartparens)
              (define-key map (kbd "(") (lambda ()
                                          (interactive)
                                          (sp-insert-pair "(")
                                          (sp-forward-slurp-sexp '(4))))
              (define-key map (kbd ")") 'close-paren)
              (define-key map (kbd "RET") (lambda ()
                                            (interactive)
                                            (newline-and-indent)
                                            (if (char-equal (char-after (point)) ?\))
                                                (save-excursion
                                                  (newline-and-indent)))
                                            (indent-according-to-mode)))
              map)))

(spacemacs/add-to-hooks 'paren-management '(lisp-mode-hook emacs-lisp-mode-hook))
