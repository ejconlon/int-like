# int-like

Newtype wrappers over `IntSet` and `IntMap`

Never mix up `Int` newtypes again! Instead of working directly with an `IntSet`, you can work with `IntLikeSet MyIntNewtype` with more or less the same API. You might have to thread through some `Coercible` constraints, though.
