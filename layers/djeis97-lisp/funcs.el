
(defun djeis97-lisp/close-paren ()
  (interactive)
  (let ((next-line-only-close-parens (save-excursion
                                       (next-line)
                                       (beginning-of-line)
                                       (looking-at "\s*\)+\s*$"))))
    (sp-forward-barf-sexp '(4))
    (save-excursion
      (next-line)
      (beginning-of-line)
      (if (and next-line-only-close-parens
               (looking-at "\s*$"))
          (ignore-errors
            (kill-whole-line)))))
  (right-char))
