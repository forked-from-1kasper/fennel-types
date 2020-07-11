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
          "function" (let [(init last) (here.init-last args-str)]
                       (.. "(" (table.concat init " × ") " → " last ")"))
          _          (.. "(" τ.constr " " (table.concat args-str " ") ")")))
      (here.variable? τ) τ.display-name
      τ))

(fn here.elim-term [term app lam variable atom]
  (if (list? term)
    (match term [f & args]
      (if (here.sym= f "λ") (lam args) (app f args)))
    (sym? term) (variable term)
    (atom term)))

here