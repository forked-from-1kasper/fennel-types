# Static typing in [Fennel](https://github.com/bakpakin/Fennel)

Example:

```lisp
(require-macros :static-typing)

(⊢ x : real)
(⊢ x ≔ 42)
```

It will be compiled to

```lua
local x = 42
return nil
```

All types are checked at compile-time and then erased.
