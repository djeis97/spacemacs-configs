;;; packages.el --- djeis97-org layer packages file for Spacemacs.
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
;; added to `djeis97-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `djeis97-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `djeis97-org/pre-init-PACKAGE' and/or
;;   `djeis97-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst djeis97-org-packages
  '((org :location built-in)
    org-caldav
    oauth2)
  "The list of Lisp packages required by the djeis97-org layer.

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

(defun djeis97-org/post-init-org ()
  (spacemacs/add-all-to-hook 'org-mode-hook 'org-indent-mode 'auto-fill-mode)
  (with-eval-after-load 'org-agenda
    (setq org-agenda-files '("~/org/")

          org-refile-use-outline-path 'file
          org-refile-targets '((nil :maxlevel . 3)
                               (org-agenda-files :maxlevel . 2))))

  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes
                 '("acmart-sigconf" "\\documentclass[sigconf]{acmart}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
                 '("koma-article"
                   "\\documentclass{scrartcl}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

(defun djeis97-org/init-org-caldav ()
  (use-package org-caldav
    :after org
    :config
    (setq
     org-caldav-oauth2-client-id "291194314842-e5dpa8pl2267noslbg56t44tehc58c62.apps.googleusercontent.com"
     org-caldav-oauth2-client-secret (with-temp-buffer
                                       (insert-file-contents-literally "~/org/CalSecret")
                                       (car (split-string (buffer-string) "\n" t)))
     org-caldav-url 'google
     org-caldav-calendars '((:calendar-id "qwe12345678910@gmail.com"
                                          :files ("~/org/cal.org"
                                                  "~/org/fromcaldav.org")
                                          :inbox "~/org/cal.org"))
     org-caldav-save-directory "~/org/caldavState/")))

(defun djeis97-org/init-oauth2 ()
  (use-package oauth2
    :demand t))

;;; packages.el ends here
