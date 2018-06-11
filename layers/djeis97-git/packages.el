
(defconst djeis97-git-packages
  '(magit
    (magit-gh-pulls :excluded t)))

(defun djeis97-git/pre-init-magit ()
  (spacemacs|use-package-add-hook magit
    :post-config
    (magit-wip-after-apply-mode)
    (magit-wip-after-save-mode)))

