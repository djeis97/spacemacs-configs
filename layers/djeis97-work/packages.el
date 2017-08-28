;;; packages.el --- djeis97-work layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Elijah <ea004237@XANA-DG-laptop>
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
;; added to `djeis97-work-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `djeis97-work/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `djeis97-work/pre-init-PACKAGE' and/or
;;   `djeis97-work/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst djeis97-work-packages
  '((python :location built-in)
    exwm)
  "The list of Lisp packages required by the djeis97-work layer.

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

(defun djeis97-work/post-init-exwm ()
  (setq exwm-djeis97-workspace-number 6)
  (require 'exwm-randr)
  (setq exwm-randr-djeis97-workspace-output-plist
        '(0 "DisplayPort-2" 1 "DisplayPort-1" 2 "DisplayPort-0" 3 "DisplayPort-2" 4 "DisplayPort-1" 5 "DisplayPort-0"))
  (exwm-randr-enable))

(defun djeis97-work/post-init-python ()
  (use-package python
    :defer t
    :mode ("\\.pyt\\'" . python-mode))
  (pushnew ".pyt.xml" completion-ignored-extensions))

;;; packages.el ends here
