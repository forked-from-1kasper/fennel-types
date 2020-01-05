(require-macros :static-typing)

;; Church numerals
(def-type-synonym nat (α → (α → α) → α))

(⊢ zero : nat)
(⊢ succ : nat → nat)

(⊢ zero ≔ (λ (z : α) (s : α → α) ↦ z))
(⊢ succ ≔ (λ (n : nat) ↦ (λ (z : α) (s : α → α) ↦ (s (n z s)))))

(show-type zero succ)