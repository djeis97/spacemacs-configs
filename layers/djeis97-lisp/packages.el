;;; packages.el --- djeis97-lisp layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Elijah Malaby <jay@archlinux>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `djeis97-lisp-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `djeis97-lisp/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `djeis97-lisp/pre-init-PACKAGE' and/or
;;   `djeis97-lisp/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst djeis97-lisp-packages
  '(evil-cleverparens
    lispy
    (adjust-parens :location local
                   :requires smartparens)
    slime
    sly
    ;; inf-clojure
    parinfer)
  "The list of Lisp packages required by the djeis97-lisp layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defmacro define-lisp-state-key (key cmd)
  `(if evil-lisp-state-global
       (define-key evil-lisp-state-map (kbd ,key)
         (evil-lisp-state-enter-command ,cmd))
     (define-key evil-lisp-state-major-mode-map (kbd ,key)
       (evil-lisp-state-enter-command ,cmd))))

(defun define-lisp-state-keys (keys)
  (dolist (k keys)
    (let ((key (car k))
          (cmd (cdr k)))
      (eval `(define-lisp-state-key ,key ,cmd)))))

(defun djeis97-lisp/pre-init-evil-cleverparens ()
  (spacemacs|use-package-add-hook evil-cleverparens
    :post-config
    (setq evil-cleverparens-use-additional-bindings nil)
    (setq evil-cleverparens-use-additional-movement-keys nil))
  (spacemacs|use-package-add-hook evil-lisp-state
    :post-config
    (progn
      (define-lisp-state-keys '(("<" . evil-cp-<)
                                (">" . evil-cp->))))))

(defun djeis97-lisp/init-lispy ()
  (use-package lispy
    :defer t
    :commands (lispy-x lh-knight/body))
  (spacemacs|use-package-add-hook evil-lisp-state
    :post-config
    (progn
      (define-lisp-state-keys '(("x" . lispy-x)
                                ("z" . lh-knight/body))))))

(defun djeis97-lisp/init-adjust-parens ()
  (use-package adjust-parens
    :defer t
    :commands (adjust-parens-mode
               lisp-indent-adjust-parens
               lisp-dedent-adjust-parens)
    :config
    (let ((func (lambda (x) (funcall #'company-indent-or-complete-common))))
      (setq adjust-parens-fallback-indent-function func
            adjust-parens-fallback-dedent-function func))))

(defun djeis97-lisp/post-init-slime ()
  (setq-default slime-lisp-implementations lisp-implementations)
  (push 'slime-highlight-edits slime-contribs))

(defun djeis97-lisp/post-init-sly ()
  (setq-default sly-lisp-implementations lisp-implementations))

(defun djeis97-lisp/init-inf-clojure ()
  (use-package inf-clojure
    :defer t
    :init
    (add-hook 'clojure-mode-hook 'inf-clojure-minor-mode)
    (defun arcadia-inf-clojure-eldoc-setup-wrapper (orig-fun &rest args))
    ;; Temporary hack that disables eldoc for inf-clojure.
    (advice-add 'inf-clojure-eldoc-setup :around #'arcadia-inf-clojure-eldoc-setup-wrapper)))

(defun djeis97-lisp/post-init-parinfer ()
  (add-hook 'clojure-mode-hook 'parinfer-mode)
  (add-hook 'inf-clojure-mode-hook 'parinfer-mode))

;;; packages.el ends here
