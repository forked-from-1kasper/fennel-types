(require-macros :static-typing)

(⊢ id : α → α)
(⊢ id (a : α) ≔ a)

(⊢ const : α × β → α)
(⊢ const (a : α) (b : β) ≔ a)

(⊢ curry : (α × β → γ) → (α → (β → γ)))
(⊢ curry (f : α × β → γ) ≔
  (λ (x : α) ↦ (λ (y : β) ↦ (f x y))))

(⊢ id-test : α)
(⊢ id-test ≔ (id 42))

(⊢ const-test : string)
(⊢ const-test ≔ (const "test" 42))

(⊢ z ≔ true)

(⊢ + : real × real → real)
(⊢ - : real × real → real)
(⊢ * : real × real → real)
(⊢ / : real × real → real)
(⊢ > : real × real → bool)

(⊢ =   : α × α → bool)
(⊢ if  : bool × α × α → α)

(⊢ not : bool → bool)

(⊢ x : real)
(⊢ x ≔ 42)

(⊢ y : real)
(⊢ y ≔ (+ (+ x 14) 88))

(⊢ z′ : bool)
(⊢ z′ ≔ (not z)) ; checks
;(set z′ 5) ; no

(⊢ unsafe-variable : bool)
(local unsafe-variable 1)

(⊢ real-id : real → real)
(⊢ real-id (a : real) ≔ a)

(⊢ fact : real → real)
(⊢ fact (n : real) ≔
  (if (= n 0) 1 (* n (fact (- n 1)))))

(⊢ fix : (α → α) → α)
(⊢ fix (f : α → α) ≔ (f (fix f)))

(⊢ fix′ : ((α → β) → α → β) → α → β)
(⊢ fix′ (f : (α → β) → α → β) (x : α) ≔ (f (fix′ f) x))

(⊢ fact′-aux (f : real → real) (n : real) ≔
  (if (= n 0) 1 (* n (f (- n 1)))))
(⊢ fact′ (n : real) ≔ (fix′ fact′-aux n))

(⊢ zero? ≔ (= 0))

(show-type fix fix′ zero?)

(print "fact" (fact 5) "fact′" (fact′ 5))