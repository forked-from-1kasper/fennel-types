(var here {})

;;; Some useful functions
;; [a b c d e f ...] ⇒ (values [a c e ...] [b d f ...])
(fn here.split-odd-array [arr]
  (assert (= (% (length arr) 2) 0) "expected odd number of arguments")
  (let [N (/ (length arr) 2)]
    (var res₁ []) (var res₂ [])
    (for [i 1 N]
      (tset res₁ i (. arr (- (* i 2) 1)))
      (tset res₂ i (. arr (* i 2))))
    (values res₁ res₂)))

;; [a b c d ...] ∧ [A B C D ...] ⇒ {a A b B c C d D ...}
(fn here.make-dict [keys vals]
  (var res {})
  (for [i 1 (length keys)]
    (tset res (. keys i) (. vals i)))
  res)

;; should be replaced with something more efficient
(fn here.copy [T]
  (if (table? T)
    (do (var res {})
        (each [id x (pairs T)]
          (tset res id (here.copy x)))
        res)
    T))

;; tbl₂ has priority here
(fn here.union [tbl₁ tbl₂]
  (var res {})
  (each [idx val (pairs tbl₁)]
    (tset res idx val))
  (each [idx val (pairs tbl₂)]
    (tset res idx val))
  res)

(fn here.pop-from-end [arr]
  (table.remove arr (length arr)))

(fn here.map-1-in-2-out [f lst]
  (var res₁ []) (var res₂ [])
  (each [_ x (ipairs lst)]
    (let [(fst snd) (f x)]
      (table.insert res₁ fst)
      (table.insert res₂ snd)))
  (values res₁ res₂))

(fn here.foreach-2 [f lst₁ lst₂]
  (assert (= (length lst₁) (length lst₂))
    "map-2-in-1-out accepts only lists with equal length")
  (for [i 1 (length lst₁)]
    (f (. lst₁ i) (. lst₂ i))))

(fn here.foreach [f lst]
  (each [_ x (ipairs lst)] (f x)))

(fn here.append [dest ...]
  (here.foreach (partial here.foreach (partial table.insert dest)) [...]))

(fn here.map [f lst]
  (var lst′ [])
  (each [_ x (ipairs lst)]
    (table.insert lst′ (f x)))
  lst′)

(fn here.init-last [lst]
  (var lst′ []) (var last nil)
  (let [last-idx (length lst)]
    (each [idx x (ipairs lst)]
      (if (= idx last-idx) (set last x)
          (table.insert lst′ x))))
  (values lst′ last))

(fn here.sym= [term template]
  (= (tostring term) template))

(fn here.sym≠ [term template]
  (not (here.sym= term template)))

(fn here.any-2 [f lst₁ lst₂]
  (var good? true) (var i 1)
  (while (and good? (<= i (length lst₁)))
    (let [(x y) (values (. lst₁ i) (. lst₂ i))]
      (set good? (f x y)))
    (set i (+ i 1)))
  good?)

(fn here.tset-truth [tbl name body]
  (tset tbl name body) true)

(fn here.get [tbl name default]
  (or (. tbl name) default))

(fn here.function? [val]
  (= (type val) :function))

(fn here.non-empty? [tbl] (not= (length tbl) 0))

(fn here.gensym-str [] (tostring (gensym)))

(fn here.warn [str]
  (io.stderr:write (.. str "\n")))

(fn here.odd?  [n] (= (% n 2) 0))
(fn here.even? [n] (= (% n 2) 1))

here