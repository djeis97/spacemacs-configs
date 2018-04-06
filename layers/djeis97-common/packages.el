;;; packages.el --- djeis97-common layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Jay <jay@XANA-Laptop-Dual>
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
;; added to `djeis97-common-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `djeis97-common/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `djeis97-common/pre-init-PACKAGE' and/or
;;   `djeis97-common/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst djeis97-common-packages
  '((sp-hungry-delete :location local)
    (anchored-transpose :location local)
    (term :location built-in)
    ivy)
  "The list of Lisp packages required by the djeis97-common layer.

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

(defun djeis97-common/init-anchored-transpose ()
  (use-package anchored-transpose
    :demand t))


(defun djeis97-common/init-sp-hungry-delete ()
  (use-package sp-hungry-delete
    :config
    (with-eval-after-load 'smartparens
      (spacemacs/add-to-hooks 'sp-hungry-delete-mode '(smartparens-enabled-hook)))))


(defun djeis97-common/post-init-term ()
  (with-eval-after-load 'term
    (setq ansi-term-color-vector
          [term term-color-black term-color-red term-color-green
                term-color-yellow term-color-blue term-color-magenta
                term-color-cyan term-color-white])))


(defun djeis97-common/pre-init-ivy ()
  (spacemacs|use-package-add-hook ivy
    :post-init
    (spacemacs/set-leader-keys
      "aa" 'counsel-linux-app)))


;;; packages.el ends here
