(var here (include :types))

;;; Common error messages
(tset here :unknown-type
  (partial string.format "unknown type “%s”"))

(tset here :unknown-variable
  (partial string.format "unknown variable “%s”"))

(fn here.mismatched-type [expr expected-type type-here]
  (string.format (.. "expression “%s” was expected to have type “%s”, "
                     "but has type “%s”")
                 (tostring expr)
                 (here.type→string expected-type)
                 (here.type→string type-here)))

(fn here.constrain-warning [wherein type-variable-name τ]
  (string.format
    (.. "warning: in “%s”: "
        "the type variable “%s” has been constrained to be type “%s”.")
    wherein type-variable-name (here.type→string τ)))

here