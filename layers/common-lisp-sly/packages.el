(defconst common-lisp-sly-packages
  '((common-lisp-snippets :toggle (configuration-layer/package-usedp 'yasnippet))
    helm
    sly
    sly-macrostep
    sly-company
    xterm-color))

(defun common-lisp-sly/init-common-lisp-snippets ()
  (spacemacs|use-package-add-hook sly
    :post-config
    (spacemacs/declare-prefix-for-mode 'lisp-mode "ms" "snippets")
    (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
      "ss" 'helm-yas-complete)))

(defun common-lisp-sly/post-init-helm ()
  (spacemacs|use-package-add-hook sly
    :post-config
    (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
      "ri" 'spacemacs/helm-sly)))

(defun common-lisp-sly/init-sly ()
  (use-package sly
    :defer t
    :init
    (spacemacs/register-repl 'sly 'sly)
    (sp-local-pair '(sly-mrepl-mode) "'" "'" :actions nil)
    (evil-set-initial-state 'sly-mrepl-mode 'insert)
    :config
    (evilified-state-evilify-map sly-inspector-mode-map
      :mode sly-inspector-mode
      :bindings
      "gr" 'sly-inspector-reinspect)
    (evilified-state-evilify-map sly-db-mode-map
      :mode sly-db-mode)
    (setq sly-autodoc-use-multiline t
          sly-complete-symbol*-fancy t
          sly-kill-without-query-p t
          sly-repl-history-remove-duplicates t
          sly-repl-history-trim-whitespaces t
          sly-net-coding-system 'utf-8-unix)
    (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
      "'" 'sly

      ;; compile
      "cc" 'sly-compile-file
      "cC" 'sly-compile-and-load-file
      "cd" 'sly-compile-defun
      "cl" 'sly-load-file
      "cn" 'sly-remove-notes
      "cr" 'sly-compile-region

      ;; evaluate
      "eb" 'sly-eval-buffer
      "ed" 'sly-eval-defun
      "ee" 'sly-eval-last-expression
      "ef" 'sly-eval-file
      "eo" 'sly-eval-last-expression-display-output
      "ep" 'sly-eval-print-last-expression
      "er" 'sly-eval-region
      "eu" 'sly-undefine-function

      ;; navigation
      "gb" 'sly-pop-find-definition-stack
      "gd" 'sly-edit-definition
      "gn" 'sly-next-note
      "gN" 'sly-previous-note

      ;; help
      "ha" 'sly-apropos
      "hA" 'sly-apropos-all
      "hb" 'sly-who-binds
      "hd" 'sly-disassemble-symbol
      "hD" 'sly-documentation-lookup
      "hf" 'sly-describe-function
      "hh" 'sly-describe-symbol
      "hH" 'sly-hyperspec-lookup
      "hm" 'sly-who-macroexpands
      "hp" 'sly-apropos-package
      "hr" 'sly-who-references
      "hs" 'sly-who-specializes
      "hS" 'sly-who-sets
      "ht" 'sly-toggle-trace-fdefinition
      "hT" 'sly-toggle-fancy-trace
      "hu" 'sly-untrace-all
      "h<" 'sly-who-calls
      "h>" 'sly-calls-who

      ;; macro
      "ma" 'sly-macroexpand-all
      "mo" 'sly-macroexpand-1

      ;; repl
      "rc" 'sly-mrepl-clear-repl
      "re" 'sly-eval-last-expression-in-repl
      "rq" 'sly-quit-lisp
      "rr" 'sly-restart-inferior-lisp
      "rs" 'sly

      ;; stickers
      "Sb" 'sly-stickers-toggle-break-on-stickers
      "SB" 'sly-stickers-clear-buffer-stickers
      "SD" 'sly-stickers-clear-defun-stickers
      "Sf" 'sly-stickers-fetch
      "SF" 'sly-stickers-forget
      "Sn" 'sly-stickers-next-sticker
      "Sp" 'sly-stickers-prev-sticker
      "Sr" 'sly-stickers-replay
      "Sr" 'sly-stickers-clear-region-stickers
      "Ss" 'sly-stickers-dwim)

    (mapc (lambda (x)
            (spacemacs/declare-prefix-for-mode 'lisp-mode (car x) (cdr x)))
          '(("mc" . "compile")
            ("me" . "evaluate")
            ("mg" . "nav")
            ("mh" . "help")
            ("mm" . "macro")
            ("mr" . "repl")
            ("mS" . "stickers"))))

  (use-package sly-mrepl
    :after sly
    :bind
    (:map sly-mrepl-mode-map
          ("<C-up>" . sly-mrepl-previous-input-or-button)
          ("<C-down>" . sly-mrepl-next-input-or-button))))

(defun common-lisp-sly/init-sly-macrostep ()
  (use-package sly-macrostep
    :after sly
    :config
    (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
      "me" 'macrostep-expand)
    ;; Steal the macrostep transient state from the emacs lisp layer, if possible...
    (when (configuration-layer/layer-used-p 'emacs-lisp)
      (spacemacs/set-leader-keys-for-major-mode 'lisp-mode
        "ms" 'spacemacs/macrostep-transient-state/body))))

(defun common-lisp-sly/pre-init-xterm-color ()
  (spacemacs|use-package-add-hook sly
    :post-config
    (add-hook 'sly-mrepl-mode-hook (lambda () (setq xterm-color-preserve-properties t)))))

(defun common-lisp-sly/init-sly-company ()
  (use-package sly-company
    :config
    (add-hook 'sly-mode-hook 'sly-company-mode)
    (spacemacs|add-company-backends
      :backends (company-files sly-company)
      :modes sly-mode)))
