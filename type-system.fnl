(var here (include :parse))

;;; Type inference and unification
(fn prune [S τ]
  (if (here.variable? τ) (here.get S τ.name τ)
      (here.complex? τ) {:constr τ.constr
                         :args (here.map (partial prune S) τ.args)}
      τ))

(fn here.unify [S T₁ T₂]
  (let [τ₁ (prune S T₁) τ₂ (prune S T₂)]
    (if (here.variable? τ₁)
        (here.tset-truth S τ₁.name τ₂)

        (and (here.operator? τ₁) (here.variable? τ₂))
        (here.unify S τ₂ τ₁)

        (and (here.primitive? τ₁) (here.primitive? τ₂))
        (= τ₁ τ₂)

        (and (here.complex? τ₁) (here.complex? τ₂)
             (= τ₁.constr τ₂.constr)
             (= (length τ₁.args) (length τ₂.args)))
        (here.any-2 (partial here.unify S) τ₁.args τ₂.args))))

(fn infer-constant-type [context salt infer value]
  (if (table? value)
      (error "non-primitive types are not implemeneted yet, sorry")
      (values value (type value))))

(fn infer-sym-type [context salt infer value]
  (let [type-here (. context (tostring value))]
    (assert type-here (here.unknown-variable (tostring value)))
    (values value type-here)))

(fn check-list [exprs expected-types types-here]
  (var S {})
  (each [id type-here (ipairs types-here)]
    (let [expected-type (. expected-types id)]
      (assert (here.unify S expected-type type-here)
        (here.mismatched-type (. exprs id) expected-type type-here))))
  S)

(fn infer-ap [context salt infer f args]
  (let [(f′ function-type) (infer context salt f)
        (args′ types-here) (here.map-1-in-2-out (partial infer context salt) args)
        expected-types (here.copy function-type.args)
        ret-type (here.pop-from-end expected-types)
        expected-args-num (length expected-types)
        given-args-num (length types-here)]
    (if (> given-args-num expected-args-num)
        (error (string.format "more than required arguments for “%s”" (tostring f)))
        (= given-args-num expected-args-num)
        (values (list f′ (unpack args′))
                (prune (check-list args expected-types types-here) ret-type))
        (< given-args-num expected-args-num)
        (do (var partial-function-type [])
            (while (not= (length expected-types) given-args-num)
              (table.insert partial-function-type (table.remove expected-types)))
            (table.insert partial-function-type ret-type)
            (values (list (sym "partial") f′ (unpack args′))
                    (prune (check-list args expected-types types-here)
                           {:constr :function
                            :args partial-function-type}))))))

(fn infer-lam [context salt infer term]
  (let [(names types full-body) (here.parse-lam salt term)
        body (table.remove full-body)
        Δcontext (here.make-dict (here.map tostring names) types)
        context′ (here.union context Δcontext)
        (body′ ret-type) (infer context′ salt body)
        (full-body′ _) (here.map-1-in-2-out
                         (partial infer context′ salt) full-body)]
    (table.insert types ret-type) (table.insert full-body′ body′)
    (values (list (sym "fn") names (unpack full-body′))
            {:constr :function :args types})))

(fn here.infer-type [context salt value]
  (here.elim-term value
    (partial infer-ap            context salt here.infer-type)
    (partial infer-lam           context salt here.infer-type)
    (partial infer-sym-type      context salt here.infer-type)
    (partial infer-constant-type context salt here.infer-type)))

;;; Type-constraint propagation
;;;  In definitions between type variables and concrete types
;;;  system chooses concrete type.
;;;  For example, if we first define “x : α”,
;;;  then set “x ≔ 42”, system will re-define type of “x” as “real”.
(fn get-constraints [S expected-type type-here]
  (if (here.variable? expected-type)
      (tset S expected-type.name type-here)
      (and (here.complex? expected-type)
           (here.complex? type-here))
      (here.foreach-2 (partial get-constraints S)
                         expected-type.args type-here.args)))

(fn constrain [def-name τ₁ τ₂]
  (var constraints {})
  (get-constraints constraints τ₁ τ₂)
  (each [τ-name value (pairs constraints)]
    (when (here.operator? value)
      (here.warn (here.constrain-warning def-name τ-name value))))
  (prune constraints τ₁))

(fn here.inplace-constrain [context def-name expected-type type-here]
  (let [τ (constrain def-name type-here expected-type)]
    (tset context def-name (constrain def-name expected-type τ))))

here