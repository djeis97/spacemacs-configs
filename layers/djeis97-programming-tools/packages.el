(defvar djeis97-programming-tools-packages
  '(company))

(defun djeis97-programming-tools/pre-init-company ()
  (spacemacs/add-to-hooks 'djeis97-programming-tools/company-tng-configure-default
                          '(prog-mode-hook text-mode-hook)))
