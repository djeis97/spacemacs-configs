;;; packages.el --- work layer packages file for Spacemacs.
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
;; added to `work-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `work/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `work/pre-init-PACKAGE' and/or
;;   `work/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst djeis97-home-packages
  '(exwm
    (xwidgete :location local)
    (dired-sync :location local)
    (epa :location built-in)
    dired-du
    dired+)
  "The list of Lisp packages required by the work layer.

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

(defun djeis97-home/pre-init-exwm ()
  (spacemacs|use-package-add-hook exwm
    :post-config
    (setq exwm-systemtray-height 11)
    (setq exwm-workspace-number 1)
    (require 'exwm-randr)
    (setq exwm-randr-workspace-output-plist '(0 "LVDS" 1 "DFP1"))
    (exwm-randr-enable)
    (add-hook 'exwm-init-hook (lambda () (start-process "Dropbox" "*Dropbox*" "dropbox")))))

(defun djeis97-home/init-xwidgete ()
  (use-package xwidgete
    :demand t))

(defun djeis97-home/init-dired-du ()
  (use-package dired-du
    :defer t))

(defun djeis97-home/init-dired-sync ()
  (use-package dired-sync
    :defer t
    :commands 'dired-do-sync))

(defun djeis97-home/init-dired+ ()
  (use-package dired+
    :defer t)
  (with-eval-after-load 'dired
    (require 'dired+)))

(defun djeis97-home/init-epa ()
  (use-package epa
    :config
    (setq epa-pinentry-mode 'loopback)
    (pinentry-start t)))

;;; packages.el ends here
