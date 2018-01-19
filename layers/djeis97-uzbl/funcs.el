
(defun djeis97-uzbl/uzbl-open-url (fifo urls &optional initial-input mess)
  (unwind-protect
      (with-temp-buffer
        (insert-file-contents urls)
        (let* ((candidates (swiper--candidates))
               (url (ivy-read
                     "Url: "
                     (reverse candidates)
                     :initial-input initial-input
                     :caller 'djeis97-uzbl/uzbl-open-url))
               (url (format "uri %s\n" (or (nth 3 (split-string url " ")) url))))
          (write-region url nil
                        fifo t 'dont)))
    (djeis97-uzbl/reset-exwm-focus)))

(defun djeis97-uzbl/reset-exwm-focus ()
  (exwm-input--update-focus (selected-window))
  (setq exwm-input--during-command nil))


