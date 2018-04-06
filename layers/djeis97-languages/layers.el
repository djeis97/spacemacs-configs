
(configuration-layer/declare-layers
 '(latex
   (coq :variables
        coq-lock-ancestors t
        coq-compile-before-require t
        proof-strict-read-only 'retract)
   haskell))
