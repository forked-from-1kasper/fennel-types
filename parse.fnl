(var here (include :conf))

;;; Convert S-expression (some type) into internal representation
(fn here.split-by-sep [term syntax-name separators]
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
          type-desc (. here.complex-types type-constr′)
          args′ (here.map parser args)]
      (assert (sym? type-constr) "invalid syntax")
      (assert (> (length args′) 1) "function type must have at least 2 arguments")
      (assert type-desc (here.unknown-type type-constr′))
      {:constr type-desc :args args′})))

(local type-variable-regex
  (string.format "^[%s]+$" here.type-variable-valid-characters))

(fn parse-type-variable [type-name salt]
  (when (type-name:match type-variable-regex)
    {:display-name type-name :name (.. type-name salt)}))

(fn split-by-arrow [term]
  (var res [])
  (let [last-but-one (- (length term) 1)]
    (each [idx val (ipairs term)]
      (if (and (= last-but-one idx) (here.sym≠ val "→"))
          (error "expected “→” before result type")
          (and (here.odd? idx) (not= last-but-one idx) (here.sym≠ val "×"))
          (error "expected “×” between argument types")
          (here.even? idx) (table.insert res val))))
  res)

(fn parse-non-arrow [parse-type salt term]
  (if (list? term)
      (parse-complex-type (partial parse-type salt) term)
      (let [term′ (tostring term)]
        (or (. here.type-synonyms term′)
            (parse-type-variable term′ salt)
            (error (here.unknown-type term′))))))

(fn here.parse-type [salt term]
  (let [args (split-by-arrow term)]
    (if (= (length args) 1)
        (parse-non-arrow here.parse-type salt (. args 1))
        {:constr :function :args (here.map (partial here.parse-type salt) args)})))

(fn parse-annotated-variable [salt term]
  (let [(_ var-list body) (here.split-by-sep term ":" {":" true})
        name (. var-list 1)]
    (assert (= (length var-list) 1) "invalid type declaration")
    (assert (sym? name) "invalid type declaration syntax")
    (values name (here.parse-type salt body))))

(fn here.parse-lam [salt term]
  (let [(_ args-ann body) (here.split-by-sep term "λ" {"↦" true})
        (args types) (here.map-1-in-2-out
                       (partial parse-annotated-variable salt) args-ann)]
    (values args types body)))

here