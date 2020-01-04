(require-macros :static-typing)

(⊢ id : α → α)
(⊢ id ≔ (λ (a : α) ↦ a))

(⊢ const : α → β → α)
(⊢ const ≔ (λ (a : α) (b : β) ↦ a))

(⊢ id-test : α)
(⊢ id-test ≔ (id 42))

(⊢ const-test : string)
(⊢ const-test ≔ (const "test" 42))

(⊢ z ≔ true)

(⊢ +   : real → real → real)
(⊢ >   : real → real → bool)
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
(⊢ real-id ≔ (λ (a : real) ↦ (+ 1 2) a))

;; Church natural numbers
(def-type-synonym nat (α → (α → α) → α))

(⊢ zero : nat)
(⊢ succ : nat → nat)

(⊢ zero ≔ (λ (z : α) (s : α → α) ↦ z))
(⊢ succ ≔ (λ (n : nat) ↦ (λ (z : α) (s : α → α) ↦ (s (n z s)))))

(print x y z)