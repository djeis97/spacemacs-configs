
(defconst djeis97-git-packages
  '(magit
    (magithub :requires (magit))
    (magit-gh-pulls :excluded t)))

(defun djeis97-git/pre-init-magit ()
  (spacemacs|use-package-add-hook magit
    :post-config
    (magit-wip-after-apply-mode)
    (magit-wip-after-save-mode)))


(defun djeis97-git/init-magithub ()
  (use-package magithub
    :after magit
    :config (magithub-feature-autoinject t)))
