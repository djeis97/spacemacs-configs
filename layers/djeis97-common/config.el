(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice select-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
