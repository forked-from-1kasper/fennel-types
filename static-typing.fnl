(local basic (include :basic))

(var *ctx* {})

;;; Type checker configuration
(local type-synonyms
  {"real"   :number
   "string" :string
   "bool"   :boolean
   "ℝ"      :number
   "𝔹"      :boolean})

(local complex-types {})

(local type-variable-valid-characters
  (.. "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ"
      "αβγδεζηθικλμνξοπρσςτυφχψω"
      "0123456789"
      "⁰¹²³⁴⁵⁶⁷⁸⁹"
      "₀₁₂₃₄₅₆₇₈₉"))

;;; Functions over types
(fn primitive-type? [τ]
  (match τ
    :nil      true
    :number   true
    :string   true
    :boolean  true
    :table    true
    :thread   true
    :userdata true
    _         false))

(fn complex-type? [τ]
  (and (table? τ) τ.constr))

(fn type-variable? [τ]
  (and (table? τ) τ.name))

(fn type-operator? [τ]
  (or (primitive-type? τ)
      (complex-type? τ)))

(fn pprint-type [τ]
  (if (complex-type? τ)
      (let [args-str (basic.map pprint-type τ.args)]
        (match τ.constr
          "function" (.. "(" (table.concat args-str " → ") ")")
          _          (.. "(" τ.constr " " (table.concat args-str " ") ")")))
      (type-variable? τ) τ.display-name
      τ))

;;; Common error messages
(local unknown-type-error
  (partial string.format "unknown type “%s”"))

(local unknown-variable-error
  (partial string.format "unknown variable “%s”"))

(fn mismatched-type-error [expr expected-type type-here]
  (string.format (.. "expression “%s” was expected to have type “%s”, "
                     "but has type “%s”")
                 (tostring expr)
                 (pprint-type expected-type)
                 (pprint-type type-here)))

(fn constrain-warning [wherein type-variable-name τ]
  (string.format
    (.. "warning: in “%s”: "
        "the type variable “%s” has been constrained to be type “%s”.")
    wherein type-variable-name (pprint-type τ)))

;;; Convert S-expression (some type) into internal representation
(fn split-by-sep [term syntax-name separators]
  (var first-part []) (var sep? false)
  (while (and (not sep?) (basic.non-empty? term))
    (let [cur (table.remove term 1)
          sep-val (. separators (tostring cur))]
      (if (and (sym? cur) sep-val)
          (set sep? sep-val)
          (table.insert first-part cur))))
  (assert sep? (string.format "invalid %s-syntax" syntax-name))
  (values sep? first-part term))

(fn parse-complex-type [parser lst]
  (match lst [type-constr & args]
    (let [type-constr′ (tostring type-constr)
          type-desc (. complex-types type-constr′)
          args′ (basic.map parser args)]
      (assert (sym? type-constr) "invalid syntax")
      (assert (> (length args′) 1) "function type must have at least 2 arguments")
      (assert type-desc (unknown-type-error type-constr′))
      {:constr type-desc :args args′})))

(local type-variable-regex
  (string.format "^[%s]+$" type-variable-valid-characters))

(fn parse-type-variable [type-name salt]
  (when (type-name:match type-variable-regex)
    {:display-name type-name :name (.. type-name salt)}))

(fn split-by-arrow [term]
  (var res [])
  (each [idx val (ipairs term)]
    (if (basic.odd? idx)  (assert (basic.sym= val "→") "invalid arrow syntax")
        (basic.even? idx) (table.insert res val)))
  res)

(fn parse-non-arrow [parse-type salt term]
  (if (list? term)
      (parse-complex-type (partial parse-type salt) term)
      (let [term′ (tostring term)]
        (or (. type-synonyms term′)
            (parse-type-variable term′ salt)
            (error (unknown-type-error term′))))))

(fn parse-type [salt term]
  (let [args (split-by-arrow term)]
    (if (= (length args) 1)
        (parse-non-arrow parse-type salt (. args 1))
        {:constr :function :args (basic.map (partial parse-type salt) args)})))

(fn parse-annotated-variable [salt term]
  (let [(_ var-list body) (split-by-sep term ":" {":" true})
        name (. var-list 1)]
    (assert (= (length var-list) 1) "invalid type declaration")
    (assert (sym? name) "invalid type declaration syntax")
    (values name (parse-type salt body))))

(fn parse-lam [salt term]
  (let [(_ args-ann body) (split-by-sep term "λ" {"↦" true})
        (args types) (basic.map-1-in-2-out
                       (partial parse-annotated-variable salt) args-ann)]
    (values args types body)))

(fn elim-term [term app lam variable atom]
  (if (list? term)
    (match term [f & args]
      (if (basic.sym= f "λ") (lam args) (app f args)))
    (sym? term) (variable term)
    (atom term)))

;;; Type inference and unification
(fn prune [S τ]
  (if (type-variable? τ) (basic.get S τ.name τ)
      (complex-type? τ) {:constr τ.constr
                         :args (basic.map (partial prune S) τ.args)}
      τ))

(fn unify [S T₁ T₂]
  (let [τ₁ (prune S T₁) τ₂ (prune S T₂)]
    (if (type-variable? τ₁)
        (basic.tset-truth S τ₁.name τ₂)

        (and (type-operator? τ₁) (type-variable? τ₂))
        (unify S τ₂ τ₁)

        (and (primitive-type? τ₁) (primitive-type? τ₂))
        (= τ₁ τ₂)

        (and (complex-type? τ₁) (complex-type? τ₂)
             (= τ₁.constr τ₂.constr)
             (= (length τ₁.args) (length τ₂.args)))
        (basic.any-2 (partial unify S) τ₁.args τ₂.args))))

(fn infer-constant-type [context salt infer value]
  (if (or (table? value) (basic.function? value))
      (error "non-primitive types are not implemeneted yet, sorry")
      (values value (type value))))

(fn infer-sym-type [context salt infer value]
  (let [type-here (. context (tostring value))]
    (assert type-here (unknown-variable-error (tostring value)))
    (values value type-here)))

(fn check-list [exprs expected-types types-here]
  (var S {})
  (each [id type-here (ipairs types-here)]
    (let [expected-type (. expected-types id)]
      (assert (unify S expected-type type-here)
        (mismatched-type-error (. exprs id) expected-type type-here))))
  S)

(fn infer-ap [context salt infer f args]
  (let [(f′ function-type) (infer context salt f)
        (args′ types-here) (basic.map-1-in-2-out (partial infer context salt) args)
        expected-types (basic.copy function-type.args)
        ret-type (basic.pop-from-end expected-types)
        expected-args-num (length expected-types)
        given-args-num (length types-here)]
    (if (> given-args-num expected-args-num)
        (error (string.format "more than required arguments for “%s”" (tostring f)))
        (= given-args-num expected-args-num)
        (values `(,f′ ,(unpack args′))
                 (prune (check-list args expected-types types-here) ret-type))
        (< given-args-num expected-args-num)
        (do (var partial-function-type [])
            (while (not= (length expected-types) given-args-num)
              (table.insert partial-function-type (table.remove expected-types)))
            (table.insert partial-function-type ret-type)
            (values `(partial ,f′ ,(unpack args′))
                     (prune (check-list args expected-types types-here)
                            {:constr :function
                             :args partial-function-type}))))))

(fn infer-lam [context salt infer term]
  (let [(names types full-body) (parse-lam salt term)
        body (table.remove full-body)
        Δcontext (basic.make-dict (basic.map tostring names) types)
        context′ (basic.union context Δcontext)
        (body′ ret-type) (infer context′ salt body)
        (full-body′ _) (basic.map-1-in-2-out
                         (partial infer context′ salt) full-body)]
    (table.insert types ret-type) (table.insert full-body′ body′)
    (values `(fn ,names ,(unpack full-body′))
             {:constr :function :args types})))

(fn infer-type [context salt value]
  (elim-term value
    (partial infer-ap            context salt infer-type)
    (partial infer-lam           context salt infer-type)
    (partial infer-sym-type      context salt infer-type)
    (partial infer-constant-type context salt infer-type)))

;;; Type-constraint propagation
;;;  In definitions between type variables and concrete types
;;;  system chooses concrete types.
;;;  For example, if we first define “x : α”,
;;;  then set “x ≔ 42”, system will re-define type of “x” as “real”.
(fn get-constraints [S expected-type type-here]
  (if (type-variable? expected-type)
      (tset S expected-type.name type-here)
      (and (complex-type? expected-type)
           (complex-type? type-here))
      (basic.foreach-2 (partial get-constraints S)
                         expected-type.args type-here.args)))

(fn constrain [def-name τ₁ τ₂]
  (var constraints {})
  (get-constraints constraints τ₁ τ₂)
  (each [τ-name value (pairs constraints)]
    (when (type-operator? value)
      (basic.warn (constrain-warning def-name τ-name value))))
  (prune constraints τ₁))

(fn inplace-constrain [context def-name expected-type type-here]
  (let [τ (constrain def-name type-here expected-type)]
    (tset context def-name (constrain def-name expected-type τ))))

(fn define-constant [names full-body]
  (assert (= (length names) 1) "cannot define multiple values")
  (let [body (table.remove full-body)
        name (. names 1)
        name-str (tostring name)
        salt (basic.gensym-str)
        (full-body′ _) (basic.map-1-in-2-out
                         (partial infer-type *ctx* salt) full-body)
        (body′ type-here) (infer-type *ctx* salt body)
        expected-type (. *ctx* name-str)]
    (if expected-type
      (do (assert (unify {} expected-type type-here)
                  (mismatched-type-error body expected-type type-here))
          (inplace-constrain *ctx* name-str expected-type type-here))
      (tset *ctx* name-str type-here))
    (table.insert full-body′ body′)
    `(local ,name ,(unpack full-body′))))

(fn declare-type [names term]
  (let [τ (parse-type (basic.gensym-str) term)]
    (each [_ name (ipairs names)]
      (tset *ctx* (tostring name) τ))))

;;; Macro syntax
;; Fennel reads ":" in tables incorrectly
(local colon ":")
(local context-commands
  {colon  declare-type
   ":="   define-constant
   "≔"   define-constant})

(fn context-syntax [...]
  (let [(func first-part second-part)
        (split-by-sep [...] "⊢" context-commands)]
    (func first-part second-part)))

(fn def-type-synonym [name term]
  (assert (sym? name) "invalid syntax")
    (tset type-synonyms (tostring name) (parse-type (basic.gensym-str) term)))

(fn print-single-type [name]
  (assert (sym? name) "invalid syntax")
  (let [name-str (tostring name)
        τ (. *ctx* name-str)]
    (assert τ (unknown-variable-error name-str))
    (basic.warn (string.format "%s : %s" name-str (pprint-type τ)))))

(fn show-type [...]
  (basic.foreach print-single-type [...]))

{"⊢" context-syntax "def-type-synonym" def-type-synonym
 "show-type" show-type}