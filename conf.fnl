(var here (include :errors))

;;; Type checker configuration
(tset here :type-synonyms
  {"real"   :number
   "string" :string
   "bool"   :boolean
   "ℝ"      :number
   "𝔹"      :boolean})

(tset here :complex-types {})

(tset here :type-variable-valid-characters
  (.. "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ"
      "αβγδεζηθικλμνξοπρσςτυφχψω"
      "0123456789"
      "⁰¹²³⁴⁵⁶⁷⁸⁹"
      "₀₁₂₃₄₅₆₇₈₉"))

here