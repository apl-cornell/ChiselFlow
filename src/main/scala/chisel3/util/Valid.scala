// See LICENSE for license details.

/** Wrappers for valid interfaces and associated circuit generators using them.
 */

package chisel3.util

import chisel3._
// TODO: remove this once we have CompileOptions threaded through the macro system.
import chisel3.core.ExplicitCompileOptions.NotStrict

import chisel3.core.{HLevel, VLabel}
import chisel3.internal.firrtl.{Node}

/** An Bundle containing data and a signal determining if it is valid */
class Valid[+T <: Data](gen: T, val vall: Label=UnknownLabel, val genl: Label=UnknownLabel) extends Bundle
{
  val valid = Output(Bool(), vall)
  val bits  = genl match {
    case UnknownLabel => Output(gen.chiselCloneType)
    case _ => Output(gen.chiselCloneType, genl)
  }
  def fire(dummy: Int = 0): Bool = valid
  override def cloneType: this.type = Valid(gen, vall).asInstanceOf[this.type]
  override def _onModuleClose: Unit = {
    super._onModuleClose
    bits match {
      case bx: Record =>
        if(valid.lbl != null) {
          valid.lbl.conf match {
            case lx: HLevel =>
              if(argIsTemp(lx.id.getRef) && (bx.elements contains lx.id.getRef.name)) {
                lx.id.setRef(bx, lx.id.getRef.name)
                // println("valid hlvl ref: " + lx.id.getRef.pprint)
                // println(s"this: ${this.getRef.pprint}")
                // println(s"bx: ${bx.getRef.pprint}")

                // //lx.id.setRef(swapTmpWithId(lx.id.getRef, this))
                // val slots = slotsToNames(lx.id.getRef)
                // val elt = bx.namesToElt(slots)
                // println(s"slots: ${slots.toString}")
                // println(s"elt: ${elt.toString}")
                // println(s"eltId: ${elt.getRef.pprint}")
                // lx.id.setRef(Node(elt))
                // lx.id.setRef(bx, lots)
                // println (s"slots pretty: ${lx.id.getRef.pprint}")
                // if(bx.elements contains lx.id.getRef.name) lx.id.setRef(bx, lx.id.getRef.name)
              }
            case lx: VLabel =>
              if(argIsTemp(lx.id.getRef) && (bx.elements contains lx.id.getRef.name))
                lx.id.setRef(bx, lx.id.getRef.name)
            case lx => 
          }
          valid.lbl.integ match {
            case lx: HLevel => 
              if(argIsTemp(lx.id.getRef) && (bx.elements contains lx.id.getRef.name))
                lx.id.setRef(bx, lx.id.getRef.name)
            case lx: VLabel =>
              if(argIsTemp(lx.id.getRef) && (bx.elements contains lx.id.getRef.name))
                lx.id.setRef(bx, lx.id.getRef.name)
            case lx => 
          }
        }
            case _ =>
    }
  }
}

/** Adds a valid protocol to any interface */
object Valid {
  def apply[T <: Data](gen: T, vall: Label=UnknownLabel, genl: Label=UnknownLabel): Valid[T] = new Valid(gen, vall, genl)
}

/** A hardware module that delays data coming down the pipeline
 by the number of cycles set by the latency parameter. Functionality
 is similar to ShiftRegister but this exposes a Pipe interface.

 Example usage:
 val pipe = new Pipe(UInt())
 pipe.io.enq <> produce.io.out
 consumer.io.in <> pipe.io.deq
 */
object Pipe
{
  def apply[T <: Data](enqValid: Bool, enqBits: T, latency: Int, pvall: Label, pgenl: Label): Valid[T] = {
    if (latency == 0) {
      val out = Wire(Valid(enqBits, vall=pvall, genl=pgenl))
      out.valid <> enqValid
      out.bits <> enqBits
      out
    } else {
      val v = Reg(Bool(), next=enqValid, init=false.B)
      val b = RegEnable(enqBits, enqValid)
      apply(v, b, latency-1, pvall, pgenl)
    }
  }
  def apply[T <: Data](enqValid: Bool, enqBits: T): Valid[T] = apply(enqValid, enqBits, 1, UnknownLabel, UnknownLabel)
  def apply[T <: Data](enqValid: Bool, enqBits: T, latency: Int): Valid[T] = apply(enqValid, enqBits, latency, UnknownLabel, UnknownLabel)
  def apply[T <: Data](enq: Valid[T], latency: Int = 1, pvall: Label = UnknownLabel, pgenl: Label=UnknownLabel): Valid[T] = apply(enq.valid, enq.bits, latency, pvall, pgenl)
  //def apply[T <: Data](enq: Valid[T], latency: Int= 1, pvall: Label, pgenl: Label): Valid[T] = apply(enq.valid, enq.bits, latency, pvall, pgenl)
}

class Pipe[T <: Data](gen: T, latency: Int = 1, pvall: Label=UnknownLabel, pgenl: Label=UnknownLabel) extends Module
{
  class PipeIO extends Bundle {
    val enq = Input(Valid(gen, pvall, pgenl))
    val deq = Output(Valid(gen, pvall, pgenl))
  }

  val io = IO(new PipeIO)

  io.deq <> Pipe(io.enq, latency)
}
