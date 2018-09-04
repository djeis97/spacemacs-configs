
(define-minor-mode paren-management
  "Bindings to better manage parens."
  :keymap (let ((map (make-sparse-keymap)))
            (when (configuration-layer/package-usedp 'adjust-parens)
              (define-key map (kbd "<tab>") 'lisp-indent-adjust-parens)
              (define-key map (kbd "<backtab>") 'lisp-dedent-adjust-parens))
            (when (configuration-layer/package-usedp 'smartparens)
              (define-key map (kbd "(") (lambda ()
                                          (interactive)
                                          (sp-insert-pair "(")
                                          (sp-forward-slurp-sexp '(4))))
              (define-key map [remap newline-and-indent] (lambda ()
                                                           (interactive)
                                                           (newline-and-indent)
                                                           (if (char-equal (char-after (point)) ?\))
                                                               (save-excursion
                                                                 (newline-and-indent)))
                                                           (indent-according-to-mode)))
              (define-key map (kbd ")") 'djeis97-lisp/close-paren)
              map)))

(spacemacs/add-to-hooks 'adjust-parens-mode '(lisp-mode-hook emacs-lisp-mode-hook clips-mode-hook))
