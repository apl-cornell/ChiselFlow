// See LICENSE for license details.

/** Arbiters in all shapes and sizes.
  */

package chisel3.util

import chisel3._
// TODO: remove this once we have CompileOptions threaded through the macro system.
import chisel3.core.ExplicitCompileOptions.NotStrict

/** IO bundle definition for an Arbiter, which takes some number of ready-valid inputs and outputs
  * (selects) at most one.
  *
  * @param gen data type
  * @param n number of inputs
  */
class ArbiterIO[T <: Data](gen: T, out_gen: T, n: Int, inl: Label, outl: Label) extends Bundle {
  val in  = Flipped(Vec(n, Decoupled(gen, inl, inl)))
  val out = Decoupled(out_gen, outl, outl)
  val chosen = Output(UInt(log2Up(n).W), outl)
  def this(gen: T, n: Int) = this(gen, gen, n, UnknownLabel, UnknownLabel)
}

/** Arbiter Control determining which producer has access
  */
object ArbiterCtrl {
  def apply(request: Seq[Bool]): Seq[Bool] = request.length match {
    case 0 => Seq()
    case 1 => Seq(true.B)
    case _ => true.B +: request.tail.init.scanLeft(request.head)(_ || _).map(!_)
  }
}

abstract class LockingArbiterLike[T <: Data](gen: T, out_gen: T, n: Int, count: Int,
    needsLock: Option[T => Bool], inl: Label, outl: Label) extends Module {
  protected def grant: Seq[Bool]
  protected def choice: UInt
  val io = IO(new ArbiterIO(gen, out_gen, n, inl, outl))

  io.chosen := choice
  io.out.valid := io.in(io.chosen).valid
  io.out.bits := io.in(io.chosen).bits

  if (count > 1) {
    val lockCount = Counter(count)
    val lockIdx = Reg(UInt())
    val locked = lockCount.value =/= 0.U
    val wantsLock = needsLock.map(_(io.out.bits)).getOrElse(true.B)

    when (io.out.fire() && wantsLock) {
      lockIdx := io.chosen
      lockCount.inc()
    }

    when (locked) { io.chosen := lockIdx }
    for ((in, (g, i)) <- io.in zip grant.zipWithIndex)
      in.ready := Mux(locked, lockIdx === i.asUInt, g) && io.out.ready
  } else {
    for ((in, g) <- io.in zip grant)
      in.ready := g && io.out.ready
  }

  def this(gen: T, n: Int, count: Int, needsLock: Option[T=>Bool])
    = this(gen, gen, n, count, needsLock, UnknownLabel, UnknownLabel)
}

class LockingRRArbiter[T <: Data](gen: T, out_gen: T, n: Int, count: Int, 
    inl: Label, outl: Label, needsLock: Option[T => Bool])
    extends LockingArbiterLike[T](gen, out_gen, n, count, needsLock, inl, outl) {
  private lazy val lastGrant = RegEnable(io.chosen, io.out.fire())
  private lazy val grantMask = (0 until n).map(_.asUInt > lastGrant)
  private lazy val validMask = io.in zip grantMask map { case (in, g) => in.valid && g }

  override protected def grant: Seq[Bool] = {
    val ctrl = ArbiterCtrl((0 until n).map(i => validMask(i)) ++ io.in.map(_.valid))
    (0 until n).map(i => ctrl(i) && grantMask(i) || ctrl(i + n))
  }

  override protected lazy val choice = Wire(init=(n-1).asUInt)
  for (i <- n-2 to 0 by -1)
    when (io.in(i).valid) { choice := i.asUInt }
  for (i <- n-1 to 1 by -1)
    when (validMask(i)) { choice := i.asUInt }

    def this(gen: T, n: Int, count: Int, needsLock: Option[T => Bool ] = None) =
      this(gen, gen, n, count, UnknownLabel, UnknownLabel, needsLock)
}

class LockingArbiter[T <: Data](gen: T, out_gen: T, n: Int, count: Int, 
  inl: Label, outl: Label, needsLock: Option[T => Bool])
    extends LockingArbiterLike[T](gen, out_gen, n, count, needsLock, inl, outl) {
  protected def grant: Seq[Bool] = ArbiterCtrl(io.in.map(_.valid))

  override protected lazy val choice = Wire(init=(n-1).asUInt)
  for (i <- n-2 to 0 by -1)
    when (io.in(i).valid) { choice := i.asUInt }

  def this(gen: T, n: Int, count: Int, needsLock: Option[T => Bool] = None) =
    this(gen, gen, n, count, UnknownLabel, UnknownLabel, needsLock)
}

/** Hardware module that is used to sequence n producers into 1 consumer.
  * Producers are chosen in round robin order.
  *
  * @example {{{
  * val arb = new RRArbiter(2, UInt())
  * arb.io.in(0) <> producer0.io.out
  * arb.io.in(1) <> producer1.io.out
  * consumer.io.in <> arb.io.out
  * }}}
  */
class RRArbiter[T <: Data](gen:T, out_gen: T, n: Int, inl: Label, outl: Label)
  extends LockingRRArbiter[T](gen, out_gen, n, 1, inl, outl, None) {
    def this(gen: T, n: Int) = this(gen, gen, n, UnknownLabel, UnknownLabel)
}

/** Hardware module that is used to sequence n producers into 1 consumer.
  * Priority is given to lower producer.
  *
  * @example {{{
  * val arb = Module(new Arbiter(2, UInt()))
  * arb.io.in(0) <> producer0.io.out
  * arb.io.in(1) <> producer1.io.out
  * consumer.io.in <> arb.io.out
  * }}}
  */
class Arbiter[T <: Data](gen: T, out_gen: T, n: Int, inl: Label, outl: Label) extends Module {
  val io = IO(new ArbiterIO(gen, out_gen, n, inl, outl))

  def this(gen: T, n: Int) = this(gen, gen, n, UnknownLabel, UnknownLabel)
  
  io.chosen := (n-1).asUInt
  io.out.bits := io.in(n-1).bits
  for (i <- n-2 to 0 by -1) {
    when (io.in(i).valid) {
      io.chosen := i.asUInt
      io.out.bits := io.in(i).bits
    }
  }

  private val grant = ArbiterCtrl(io.in.map(_.valid))
  for ((in, g) <- io.in zip grant)
    in.ready := g && io.out.ready
  io.out.valid := !grant.last || io.in.last.valid
}
