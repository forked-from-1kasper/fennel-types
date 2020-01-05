(require-macros :static-typing)

;; Church numerals
(def-type-synonym nat ((α → α) → α → α))

(⊢ zero : nat)
(⊢ succ : nat → nat)
(⊢ mult : nat → nat → nat)
(⊢ exp  : nat → nat → nat)

(⊢ zero (s : α → α) (z : α) ≔ z)
(⊢ succ (n : nat) ≔ (λ (s : α → α) (z : α) ↦ (s (n s z))))
(⊢ mult (m : nat) (n : nat) ≔
  (λ (s : α → α) (z : α) ↦ (m (n s) z)))
(⊢ exp (m : nat) (n : nat) ≔
  (λ (s : α → α) (z : α) ↦
    ((n (λ (f : α → α) ↦ (m f)) s) z)))

(⊢ + : real → real → real)
(⊢ - : real → real → real)
(⊢ inc ≔ (+ 1))

(⊢ nat→real (n : nat) ≔ (n inc 0))

(⊢ =   : α → α → bool)
(⊢ if  : bool → α → α → α)

(⊢ real→nat : real → nat)
(⊢ real→nat (n : real) ≔
  (if (= n 0) zero
      (succ (real→nat (- n 1)))))

(⊢ one   ≔ (succ zero))
(⊢ two   ≔ (succ one))
(⊢ three ≔ (succ two))

(show-type zero succ mult exp)
(print (string.format "2³ = %d\n2 × 3 = %d"
         (nat→real (exp (real→nat 2) three))
         (nat→real (mult two three))))