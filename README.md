# int-like

[![CircleCI](https://circleci.com/gh/ejconlon/int-like/tree/master.svg?style=svg)](https://circleci.com/gh/ejconlon/int-like/tree/master)

Newtype wrappers over `IntSet` and `IntMap`

Never mix up `Int` newtypes again! Instead of working directly with an `IntSet`, you can work with `IntLikeSet MyIntNewtype` with more or less the same API. You might have to thread through some `Coercible` constraints, though.
