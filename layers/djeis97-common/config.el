(defvar *switch-buffer-save-buffer-tests* nil)

(defun should-save-buffer (from to)
  (let ((tests (mapcar (lambda (f) (funcall f from to)) *switch-buffer-save-buffer-tests*)))
    (and (some (lambda (v) (eq v :do)) tests)
         (notany (lambda (v) (eq v :dont)) tests))))

(defadvice switch-to-buffer (before save-buffer-now activate)
  (when (should-save-buffer (current-buffer) (get-buffer (ad-get-arg 0)))
    (save-buffer)))

(defadvice select-window (before other-window-now activate)
  (unless (ad-get-arg 1) ;; norecord- probably wasn't me
    (let* ((other (window-buffer (ad-get-arg 0))))
      (when (should-save-buffer (current-buffer) other)
        (save-buffer)))))

(defun save-buffer-is-file-test (this-buffer other-buffer)
  (unless (buffer-file-name this-buffer) :dont))
(pushnew 'save-buffer-is-file-test *switch-buffer-save-buffer-tests*)
(defun save-buffer-same-buffer-test (this-buffer other-buffer)
  (when (eq this-buffer other-buffer) :dont))
(pushnew 'save-buffer-same-buffer-test *switch-buffer-save-buffer-tests*)
(defun save-buffer-other-is-file-test (other-window)
  (when (buffer-file-name (window-buffer other-window)) :do))
(pushnew 'save-buffer-is-file-test *switch-buffer-save-buffer-tests*)
(defun save-buffer-other-is-mode-test (this-buffer other-buffer)
  (with-current-buffer other-buffer
    (when (derived-mode-p 'sly-mrepl-mode 'help-mode) :do)))
(pushnew 'save-buffer-other-is-mode-test *switch-buffer-save-buffer-tests*)

(with-eval-after-load 'dired
  (setq dired-omit-files "^\\.[^.]\\|^#.*#$"))

(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-use-ssh-controlmaster-options nil))


(setq-default
 browse-url-generic-program "uzbl-browser"
 browse-url-browser-function 'browse-url-generic)
