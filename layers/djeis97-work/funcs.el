
(defun djeis97/inactive-minibuffer-settings ()
  (setq evil-no-display t)
  (evil-insert-state)
  (smartparens-strict-mode))

(defun djeis97-work/lock-display (message)
  (interactive "s")
  (start-process-shell-command "lock" nil (format "cinnamon-screensaver-command -l -m '%s 😸'" message)))

