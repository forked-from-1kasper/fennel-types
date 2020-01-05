(var basic {})

;;; Some useful functions
;; [a b c d e f ...] ⇒ (values [a c e ...] [b d f ...])
(fn basic.split-odd-array [arr]
  (assert (= (% (length arr) 2) 0) "expected odd number of arguments")
  (let [N (/ (length arr) 2)]
    (var res₁ []) (var res₂ [])
    (for [i 1 N]
      (tset res₁ i (. arr (- (* i 2) 1)))
      (tset res₂ i (. arr (* i 2))))
    (values res₁ res₂)))

;; [a b c d ...] ∧ [A B C D ...] ⇒ {a A b B c C d D ...}
(fn basic.make-dict [keys vals]
  (var res {})
  (for [i 1 (length keys)]
    (tset res (. keys i) (. vals i)))
  res)

;; should be replaced with something more efficient
(fn basic.copy [T]
  (if (table? T)
    (do (var res {})
        (each [id x (pairs T)]
          (tset res id (basic.copy x)))
        res)
    T))

;; tbl₂ has priority here
(fn basic.union [tbl₁ tbl₂]
  (var res {})
  (each [idx val (pairs tbl₁)]
    (tset res idx val))
  (each [idx val (pairs tbl₂)]
    (tset res idx val))
  res)

(fn basic.pop-from-end [arr]
  (table.remove arr (length arr)))

(fn basic.map-1-in-2-out [f lst]
  (var res₁ []) (var res₂ [])
  (each [_ x (ipairs lst)]
    (let [(fst snd) (f x)]
      (table.insert res₁ fst)
      (table.insert res₂ snd)))
  (values res₁ res₂))

(fn basic.foreach-2 [f lst₁ lst₂]
  (assert (= (length lst₁) (length lst₂))
    "map-2-in-1-out accepts only lists with equal length")
  (for [i 1 (length lst₁)]
    (f (. lst₁ i) (. lst₂ i))))

(fn basic.foreach [f lst]
  (each [_ x (ipairs lst)] (f x)))

(fn basic.map [f lst]
  (var lst′ [])
  (each [_ x (ipairs lst)]
    (table.insert lst′ (f x)))
  lst′)

(fn basic.sym= [term template]
  (= (tostring term) template))

(fn basic.any-2 [f lst₁ lst₂]
  (var good? true) (var i 1)
  (while (and good? (<= i (length lst₁)))
    (let [(x y) (values (. lst₁ i) (. lst₂ i))]
      (set good? (f x y)))
    (set i (+ i 1)))
  good?)

(fn basic.tset-truth [tbl name body]
  (tset tbl name body) true)

(fn basic.get [tbl name default]
  (or (. tbl name) default))

(fn basic.function? [val]
  (= (type val) :function))

(fn basic.non-empty? [tbl] (not= (length tbl) 0))

(fn basic.gensym-str [] (tostring (gensym)))

(fn basic.warn [str]
  (io.stderr:write (.. str "\n")))

(fn basic.odd?  [n] (= (% n 2) 0))
(fn basic.even? [n] (= (% n 2) 1))

basic