
(defvar lisp-implementations '((sbcl-large ("sbcl" "--dynamic-space-size" "6144"))
                               (sbcl ("sbcl"))
                               (ros ("ros" "run"))
                               (ros-local ("ros" "-S" "." "run"))))
