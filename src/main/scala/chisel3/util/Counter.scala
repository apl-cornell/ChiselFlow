// See LICENSE for license details.

package chisel3.util

import chisel3._
//import chisel3.core.ExplicitCompileOptions.Strict

/** A counter module
 *
  * @param n number of counts before the counter resets (or one more than the
  * maximum output value of the counter), need not be a power of two
  */
class Counter(val n: Int, l: Label=UnknownLabel) {
  require(n >= 0)
  val value = if (n > 1) Reg(init=0.U(log2Up(n).W), lbl = l) else 0.U

  /** Increment the counter, returning whether the counter currently is at the
    * maximum and will wrap. The incremented value is registered and will be
    * visible on the next cycle.
    */
  def inc(): Bool = {
    if (n > 1) {
      val wrap = value === (n-1).asUInt
      value := value + 1.U
      if (!isPow2(n)) {
        when (wrap) { value := 0.U }
      }
      wrap
    } else {
      true.B
    }
  }
}

object Counter
{
  /** Instantiate a [[Counter! counter]] with the specified number of counts.
    */
  def apply(n: Int, l: Label = UnknownLabel): Counter = new Counter(n, l)

  /** Instantiate a [[Counter! counter]] with the specified number of counts and a gate.
   *
    * @param cond condition that controls whether the counter increments this cycle
    * @param n number of counts before the counter resets
    * @return tuple of the counter value and whether the counter will wrap (the value is at
    * maximum and the condition is true).
    *
    * @example {{{
    * val countOn = true.B // increment counter every clock cycle
    * val (counterValue, counterWrap) = Counter(countOn, 4)
    * when (counterValue === 3.U) {
    *   ...
    * }
    * }}}
    */
  def apply(cond: Bool, n: Int): (UInt, Bool) = {
    val c = new Counter(n)
    var wrap: Bool = null
    when (cond) { wrap = c.inc() }
    (c.value, cond && wrap)
  }
}
