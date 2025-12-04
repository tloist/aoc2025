package common

import scala.math.Numeric.Implicits.infixNumericOps

package object dimension2:
  extension [N: Numeric](n: N)
    private def num = summon[Numeric[N]]
    def until(end: N): LazyList[N] = LazyList.iterate(n) { c => c + num.one }.takeWhile(num.lt(_, end))
    def to(end: N): LazyList[N] = until(end).appended(end)
