(var here (include :basic))

;;; Functions over types
(fn here.primitive? [τ]
  (match τ
    :nil      true
    :number   true
    :string   true
    :boolean  true
    :table    true
    :thread   true
    :userdata true
    _         false))

(fn here.complex? [τ]
  (and (table? τ) τ.constr))

(fn here.variable? [τ]
  (and (table? τ) τ.name))

(fn here.operator? [τ]
  (or (here.primitive? τ)
      (here.complex? τ)))

(fn here.type→string [τ]
  (if (here.complex? τ)
      (let [args-str (here.map here.type→string τ.args)]
        (match τ.constr
          "function" (.. "(" (table.concat args-str " → ") ")")
          _          (.. "(" τ.constr " " (table.concat args-str " ") ")")))
      (here.variable? τ) τ.display-name
      τ))

here