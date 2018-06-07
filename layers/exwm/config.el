(defvar exwm--terminal-command "xterm"
  "Terminal command to run.")

(defvar exwm--locking-command "lock"
  "Command to run when locking session")

(defvar exwm-app-launcher--prompt "$ "
  "Prompt for the EXWM application launcher")

(defvar exwm--hide-tiling-modeline nil
  "Whether to hide modeline.")

(defvar exwm-workspace-minibuffer-position 'bottom)

(defun exwm-passthrough (orig-fun keymap on-exit &optional foreign-keys)
  (setq exwm-input-line-mode-passthrough t)
  (let ((on-exit (lexical-let ((on-exit on-exit))
                   (lambda ()
                     (setq exwm-input-line-mode-passthrough nil)
                     (when on-exit (funcall on-exit))))))
    (funcall orig-fun keymap on-exit foreign-keys)))

(advice-add 'hydra-set-transient-map :around #'exwm-passthrough)
