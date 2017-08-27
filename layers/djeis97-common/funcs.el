
(defun eshell/catb (name)
  (let ((b (get-buffer name)))
    (if b
        (with-current-buffer b
          (buffer-string))
      (eshell/cat name))))

(defun pcomplete/eshell-mode/catb ()
  (pcomplete-here (append (mapcar #'buffer-name (buffer-list))
                          (directory-files default-directory))))

(defun mailto (to)
  (require 'gnus)
  (unless gnus-active-hashtb (gnus))
  (browse-url-mail to))

