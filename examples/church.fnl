(require-macros :static-typing)

;; Church numerals
(def-type-synonym nat ((α → α) → α → α))

(⊢ zero : nat)
(⊢ succ : nat → nat)
(⊢ mult : nat → nat → nat)
(⊢ exp  : nat → nat → nat)

(⊢ zero ≔ (λ (s : α → α) (z : α) ↦ z))
(⊢ succ ≔ (λ (n : nat) ↦ (λ (s : α → α) (z : α) ↦ (s (n s z)))))
(⊢ mult ≔ (λ (m : nat) (n : nat) ↦
             (λ (s : α → α) (z : α) ↦ (m (n s) z))))
(⊢ exp  ≔ (λ (m : nat) (n : nat) ↦
             (λ (s : α → α) (z : α) ↦
               ((n (λ (f : α → α) ↦ (m f)) s) z))))

(⊢ + : real → real → real)
(⊢ - : real → real → real)
(⊢ inc ≔ (+ 1))

(⊢ nat-to-real ≔ (λ (n : nat) ↦ (n inc 0)))

(⊢ =   : α → α → bool)
(⊢ if  : bool → α → α → α)

(⊢ real-to-nat : real → nat)
(⊢ real-to-nat ≔ (λ (n : real) ↦
                    (if (= n 0) zero
                        (succ (real-to-nat (- n 1))))))

(⊢ one   ≔ (succ zero))
(⊢ two   ≔ (succ one))
(⊢ three ≔ (succ two))

(show-type zero succ mult exp)
(print (string.format "2³ = %d\n2 × 3 = %d"
         (nat-to-real (exp (real-to-nat 2) three))
         (nat-to-real (mult two three))))