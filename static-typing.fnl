(var *ctx* {})

;;; Some useful functions
;; [a b c d e f ...] ⇒ (values [a c e ...] [b d f ...])
(fn split-odd-array [arr]
  (assert (= (% (length arr) 2) 0) "expected odd number of arguments")
  (let [N (/ (length arr) 2)]
    (var res₁ []) (var res₂ [])
    (for [i 1 N]
      (tset res₁ i (. arr (- (* i 2) 1)))
      (tset res₂ i (. arr (* i 2))))
    (values res₁ res₂)))

;; [a b c d ...] ∧ [A B C D ...] ⇒ {a A b B c C d D ...}
(fn make-dict [keys vals]
  (var res {})
  (for [i 1 (length keys)]
    (tset res (. keys i) (. vals i)))
  res)

;; should be replaced with something more efficient
(fn copy [T]
  (if (table? T)
    (do (var res {})
        (each [id x (pairs T)]
          (tset res id (copy x)))
        res)
    T))

;; tbl₂ has priority here
(fn union [tbl₁ tbl₂]
  (var res {})
  (each [idx val (pairs tbl₁)]
    (tset res idx val))
  (each [idx val (pairs tbl₂)]
    (tset res idx val))
  res)

(fn pop-last [arr]
  (table.remove arr (length arr)))

(fn push-last [arr x]
  (table.insert arr (length arr) x))

(fn map-1-in-2-out [f lst]
  (var res₁ []) (var res₂ [])
  (each [_ x (ipairs lst)]
    (let [(fst snd) (f x)]
      (table.insert res₁ fst)
      (table.insert res₂ snd)))
  (values res₁ res₂))

(fn foreach-2 [f lst₁ lst₂]
  (assert (= (length lst₁) (length lst₂))
    "map-2-in-1-out accepts only lists with equal length")
  (for [i 1 (length lst₁)]
    (f (. lst₁ i) (. lst₂ i))))

(fn map [f lst]
  (var lst′ [])
  (each [_ x (ipairs lst)]
    (table.insert lst′ (f x)))
  lst′)

(fn sym= [term template]
  (= (tostring term) template))

(fn any-2 [f lst₁ lst₂]
  (var good? true) (var i 1)
  (while (and good? (<= i (length lst₁)))
    (let [(x y) (values (. lst₁ i) (. lst₂ i))]
      (set good? (f x y)))
    (set i (+ i 1)))
  good?)

(fn tset-truth [tbl name body]
  (tset tbl name body) true)

(fn get [tbl name default]
  (or (. tbl name) default))

(fn function? [val]
  (= (type val) :function))

(fn non-empty? [tbl] (not= (next tbl) nil))

(fn gensym-str [] (tostring (gensym)))

(fn warn [str]
  (io.stderr:write (.. str "\n")))

;;; Type checker configuration
(local primitive-types
  {"real"   :number
   "string" :string
   "bool"   :boolean
   "ℝ"      :number
   "𝔹"      :boolean})

(local complex-types
  {"->" :function
   "→"  :function})

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
      (string.format "(%s %s)" τ.constr
        (table.concat (map pprint-type τ.args) " "))
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
(fn parse-complex-type [parser lst]
  (match lst [type-constr & args]
    (let [type-constr′ (tostring type-constr)
          type-desc (. complex-types type-constr′)
          args′ (map parser args)]
      (assert (sym? type-constr) "invalid syntax")
      (assert (> (length args′) 1) "function type must have at least 2 arguments")
      (assert type-desc (unknown-type-error type-constr′))
      {:constr type-desc :args args′})))

(local type-variable-regex
  (string.format "^[%s]+$" type-variable-valid-characters))

(fn parse-type-variable [type-name salt]
  (when (type-name:match type-variable-regex)
    {:display-name type-name :name (.. type-name salt)}))

(fn parse-type [salt type]
  (let [type′ (tostring type)]
    (if (sym? type)
        (or (. primitive-types type′)
            (parse-type-variable type′ salt)
            (error (unknown-type-error type′)))
        (list? type) (parse-complex-type (partial parse-type salt) type)
        (error (unknown-type-error type′)))))

;;; S-expression parsing
(fn parse-annotated-variable [salt term]
  (assert (= (length term) 3)
          "invalid type declaration syntax")
  (match term [name sep τ]
    (do (assert (sym= sep ":") "“:” was not found in type declaration")
        (assert (sym? name) (string.format "“%s” is not a variable name"
                                           (tostring name)))
        (values name (parse-type salt τ)))))

(fn parse-lam [salt body]
  (var args-ann []) (var sep? false)
  (while (and (not sep?) (non-empty? body))
    (let [arg (table.remove body 1)]
      (if (sym= arg "↦")
          (set sep? true)
          (table.insert args-ann arg))))

  (let [(args types)
        (map-1-in-2-out (partial parse-annotated-variable salt) args-ann)]
    (assert sep? "invalid λ-syntax")
    (values args types body)))

(fn elim-term [term app lam variable atom]
  (if (list? term)
    (match term [f & args]
      (if (sym= f "λ") (lam args) (app f args)))
    (sym? term) (variable term)
    (atom term)))

;;; Type inference and unification
(fn prune [S τ]
  (if (type-variable? τ) (get S τ.name τ)
      (complex-type? τ) {:constr τ.constr :args (map (partial prune S) τ.args)}
      τ))

(fn unify [S T₁ T₂]
  (let [τ₁ (prune S T₁) τ₂ (prune S T₂)]
    (if (type-variable? τ₁)
        (tset-truth S τ₁.name τ₂)

        (and (type-operator? τ₁) (type-variable? τ₂))
        (unify S τ₂ τ₁)

        (and (primitive-type? τ₁) (primitive-type? τ₂))
        (= τ₁ τ₂)

        (and (complex-type? τ₁) (complex-type? τ₂)
             (= τ₁.constr τ₂.constr)
             (= (length τ₁.args) (length τ₂.args)))
        (any-2 (partial unify S) τ₁.args τ₂.args))))

(fn infer-constant-type [context salt infer value]
  (if (or (table? value) (function? value))
      (error "non-primitive types are not implemeneted yet, sorry")
      (values value (type value))))

(fn infer-sym-type [context salt infer value]
  (let [type-here (. context (tostring value))]
    (assert type-here (unknown-variable-error (tostring value)))
    (values value type-here)))

(fn infer-ap [context salt infer f args]
  (let [(f′ function-type) (infer context salt f)
        (args′ types) (map-1-in-2-out (partial infer context salt) args)]
    (let [arg-types (copy function-type.args)
          ret-type (pop-last arg-types)]
      (assert (= (length arg-types) (length types))
              (string.format "not enough or more than required arguments for “%s”"
                             (tostring f)))
      (var S {})
      (each [id type-here (ipairs types)]
        (let [expected-type (. arg-types id)]
          (assert (unify S expected-type type-here)
            (mismatched-type-error (. args id) expected-type type-here))))
      (values `(,f′ ,(unpack args′)) (prune S ret-type)))))

(fn infer-lam [context salt infer term]
  (let [(names types full-body) (parse-lam salt term)
        body (table.remove full-body)
        Δcontext (make-dict (map tostring names) types)
        context′ (union context Δcontext)
        (body′ ret-type) (infer context′ salt body)
        (full-body′ _) (map-1-in-2-out (partial infer context salt) full-body)]
    (push-last types ret-type)
    (values `(fn ,names ,(unpack full-body′) ,body′)
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
  (if (and (type-variable? expected-type)
           (type-operator? type-here))
      (tset S expected-type.name type-here)
      (and (complex-type? expected-type)
           (complex-type? type-here))
      (foreach-2 (partial get-constraints S)
                 expected-type.args type-here.args)))

(fn constrain [def-name expected-type type-here]
  (var constraints {})
  (get-constraints constraints expected-type type-here)
  (each [key value (pairs constraints)]
    (warn (constrain-warning def-name key value)))
  (prune constraints expected-type))

(fn inplace-constrain [context def-name expected-type type-here]
  (let [τ (constrain def-name type-here expected-type)]
    (tset context def-name (constrain def-name expected-type τ))))

(fn define-constant [name value]
  (let [name-str (tostring name)
        salt (gensym-str)
        (value′ type-here) (infer-type *ctx* salt value)
        expected-type (. *ctx* name-str)]
    (if expected-type
      (do (assert (unify {} expected-type type-here)
                  (mismatched-type-error value expected-type type-here))
          (inplace-constrain *ctx* name-str expected-type type-here))
      (tset *ctx* name-str type-here))
    `(local ,name ,value′)))

(fn declare-type [name τ]
  (tset *ctx* (tostring name) (parse-type (gensym-str) τ)))

;;; Macro syntax
;; Fennel reads ":" in tables incorrectly
(local colon ":")
(local context-commands
  {colon  declare-type
   ":="   define-constant
   "≔"   define-constant})

(fn context-syntax [variable sep body]
  (assert (sym? sep) "invalid ⊢-syntax")
  (let [sep′ (tostring sep)
        func (. context-commands sep′)]
    (assert func "unknown command")
    (func variable body)))

{"⊢" context-syntax}