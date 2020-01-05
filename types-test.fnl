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
(⊢ -   : real → real → real)
(⊢ *   : real → real → real)
(⊢ /   : real → real → real)
(⊢ >   : real → real → bool)

(⊢ =   : α → α → bool)
(⊢ if  : bool → α → α → α)

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

(⊢ fix : (α → α) → α)
(⊢ fix ≔ (λ (f : α → α) ↦ (f (fix f))))

(⊢ fix′ : ((α → β) → α → β) → α → β)
(⊢ fix′ ≔ (λ (f : (α → β) → α → β) (x : α) ↦
             (f (fix′ f) x)))

(⊢ fact : real → real)
(⊢ fact ≔ (λ (n : real) ↦
             (if (= n 0) 1 (* n (fact (- n 1))))))

(⊢ fact′-aux ≔
  (λ (f : real → real) (n : real) ↦
    (if (= n 0) 1 (* n (f (- n 1))))))
(⊢ fact′ ≔ (λ (n : real) ↦ (fix′ fact′-aux n)))

(⊢ zero : nat)
(⊢ succ : nat → nat)

(⊢ zero ≔ (λ (z : α) (s : α → α) ↦ z))
(⊢ succ ≔ (λ (n : nat) ↦ (λ (z : α) (s : α → α) ↦ (s (n z s)))))

(⊢ zero? ≔ (= 0))

(show-type fix fix′ zero succ zero?)

(print "fact" (fact 5) "fact′" (fact′ 5))