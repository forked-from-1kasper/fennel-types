(var here (include :errors))
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

;;; Convert S-expression (some type) into internal representation
(fn split-by-sep [term syntax-name separators]
  (var first-part []) (var sep? false)
  (while (and (not sep?) (here.non-empty? term))
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
          args′ (here.map parser args)]
      (assert (sym? type-constr) "invalid syntax")
      (assert (> (length args′) 1) "function type must have at least 2 arguments")
      (assert type-desc (here.unknown-type type-constr′))
      {:constr type-desc :args args′})))

(local type-variable-regex
  (string.format "^[%s]+$" type-variable-valid-characters))

(fn parse-type-variable [type-name salt]
  (when (type-name:match type-variable-regex)
    {:display-name type-name :name (.. type-name salt)}))

(fn split-by-arrow [term]
  (var res [])
  (each [idx val (ipairs term)]
    (if (here.odd? idx)  (assert (here.sym= val "→") "invalid arrow syntax")
        (here.even? idx) (table.insert res val)))
  res)

(fn parse-non-arrow [parse-type salt term]
  (if (list? term)
      (parse-complex-type (partial parse-type salt) term)
      (let [term′ (tostring term)]
        (or (. type-synonyms term′)
            (parse-type-variable term′ salt)
            (error (here.unknown-type term′))))))

(fn parse-type [salt term]
  (let [args (split-by-arrow term)]
    (if (= (length args) 1)
        (parse-non-arrow parse-type salt (. args 1))
        {:constr :function :args (here.map (partial parse-type salt) args)})))

(fn parse-annotated-variable [salt term]
  (let [(_ var-list body) (split-by-sep term ":" {":" true})
        name (. var-list 1)]
    (assert (= (length var-list) 1) "invalid type declaration")
    (assert (sym? name) "invalid type declaration syntax")
    (values name (parse-type salt body))))

(fn parse-lam [salt term]
  (let [(_ args-ann body) (split-by-sep term "λ" {"↦" true})
        (args types) (here.map-1-in-2-out
                       (partial parse-annotated-variable salt) args-ann)]
    (values args types body)))

(fn elim-term [term app lam variable atom]
  (if (list? term)
    (match term [f & args]
      (if (here.sym= f "λ") (lam args) (app f args)))
    (sym? term) (variable term)
    (atom term)))

;;; Type inference and unification
(fn prune [S τ]
  (if (here.variable? τ) (here.get S τ.name τ)
      (here.complex? τ) {:constr τ.constr
                         :args (here.map (partial prune S) τ.args)}
      τ))

(fn unify [S T₁ T₂]
  (let [τ₁ (prune S T₁) τ₂ (prune S T₂)]
    (if (here.variable? τ₁)
        (here.tset-truth S τ₁.name τ₂)

        (and (here.operator? τ₁) (here.variable? τ₂))
        (unify S τ₂ τ₁)

        (and (here.primitive? τ₁) (here.primitive? τ₂))
        (= τ₁ τ₂)

        (and (here.complex? τ₁) (here.complex? τ₂)
             (= τ₁.constr τ₂.constr)
             (= (length τ₁.args) (length τ₂.args)))
        (here.any-2 (partial unify S) τ₁.args τ₂.args))))

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
      (assert (unify S expected-type type-here)
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
        Δcontext (here.make-dict (here.map tostring names) types)
        context′ (here.union context Δcontext)
        (body′ ret-type) (infer context′ salt body)
        (full-body′ _) (here.map-1-in-2-out
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

(fn inplace-constrain [context def-name expected-type type-here]
  (let [τ (constrain def-name type-here expected-type)]
    (tset context def-name (constrain def-name expected-type τ))))

(fn define-constant [name full-body]
  (let [body (table.remove full-body)
        name-str (tostring name)
        salt (here.gensym-str)
        (full-body′ _) (here.map-1-in-2-out
                         (partial infer-type *ctx* salt) full-body)
        (body′ type-here) (infer-type *ctx* salt body)
        expected-type (. *ctx* name-str)]
    (if expected-type
      (do (assert (unify {} expected-type type-here)
                  (here.mismatched-type body expected-type type-here))
          (inplace-constrain *ctx* name-str expected-type type-here))
      (tset *ctx* name-str type-here))
    (table.insert full-body′ body′)
    `(local ,name ,(unpack full-body′))))

(fn define-sugar [names full-body]
  (match names [name & args]
    (define-constant name
      (if (here.non-empty? args)
          (do (var lambda-term `(λ))
              (here.append lambda-term args [(sym "↦")] full-body)
              [lambda-term])
          full-body))))

(fn declare-type [names term]
  (let [τ (parse-type (here.gensym-str) term)]
    (each [_ name (ipairs names)]
      (tset *ctx* (tostring name) τ))))

;;; Macro syntax
;; Fennel reads ":" in tables incorrectly
(local colon ":")
(local context-commands
  {colon  declare-type
   ":="   define-sugar
   "≔"   define-sugar})

(fn context-syntax [...]
  (let [(func first-part second-part)
        (split-by-sep [...] "⊢" context-commands)]
    (func first-part second-part)))

(fn def-type-synonym [name term]
  (assert (sym? name) "invalid syntax")
    (tset type-synonyms (tostring name) (parse-type (here.gensym-str) term)))

(fn print-single-type [name]
  (assert (sym? name) "invalid syntax")
  (let [name-str (tostring name)
        τ (. *ctx* name-str)]
    (assert τ (here.unknown-variable name-str))
    (here.warn (string.format "%s : %s" name-str (here.type→string τ)))))

(fn show-type [...]
  (here.foreach print-single-type [...]))

{"⊢" context-syntax "def-type-synonym" def-type-synonym
 "show-type" show-type}