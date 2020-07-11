(var here (include :type-system))
(var *ctx* {})

(fn define-constant [name full-body]
  (let [body (table.remove full-body)
        name-str (tostring name)
        salt (here.gensym-str)
        (full-body′ _) (here.map-1-in-2-out
                         (partial here.infer-type *ctx* salt) full-body)
        (body′ type-here) (here.infer-type *ctx* salt body)
        expected-type (. *ctx* name-str)]
    (if expected-type
      (do (assert (here.unify {} expected-type type-here)
                  (here.mismatched-type body expected-type type-here))
          (here.inplace-constrain *ctx* name-str expected-type type-here))
      (tset *ctx* name-str type-here))
    (table.insert full-body′ body′)
    `(global ,name ,(unpack full-body′))))

(fn define-sugar [names full-body]
  (match names [name & args]
    (define-constant name
      (if (here.non-empty? args)
          (do (var lambda-term `(λ))
              (here.append lambda-term args [(sym "↦")] full-body)
              [lambda-term])
          full-body))))

(fn declare-type [names term]
  (let [τ (here.parse-type (here.gensym-str) term)]
    (each [_ name (ipairs names)]
      (tset *ctx* (tostring name) τ))))

;;; Macro syntax
(local context-commands
  {":"    declare-type
   ":="   define-sugar
   "≔"   define-sugar})

(fn context-syntax [...]
  (let [(func first-part second-part)
        (here.split-by-sep [...] "⊢" context-commands)]
    (func first-part second-part)))

(fn def-type-synonym [name term]
  (assert (sym? name) "invalid syntax")
    (tset here.type-synonyms (tostring name)
      (here.parse-type (here.gensym-str) term)))

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