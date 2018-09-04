
(defun eshell/catb (name)
  (let ((b (get-buffer name)))
    (if b
        (with-current-buffer b
          (buffer-string))
      (eshell/cat name))))

(defun pcomplete/eshell-mode/catb ()
  (pcomplete-here (append (mapcar #'buffer-name (buffer-list))
                          (directory-files default-directory))))

(defun eshell/dwim (&rest args)
  (if (not args) (message "No args given")
    (let* ((guess (dired-guess-default args))
           (candidate (if (listp guess) (car guess) guess)))
      (eshell-smart-maybe-jump-to-end)
      (insert-and-inherit (concat candidate " " (car args) " &")))))

(defun mailto (to)
  (require 'gnus)
  (unless gnus-active-hashtb (gnus))
  (browse-url-mail to))


(defun djeis97/ivy-complete-ivy-action ()
  (interactive)
  (let* ((actions (cdr (ivy-state-action ivy-last)))
         (action (ido-completing-read "Action:" (mapcar #'third actions))))
    (ivy-exit-with-action (second (find action actions :key #'third :test #'string=)))))
