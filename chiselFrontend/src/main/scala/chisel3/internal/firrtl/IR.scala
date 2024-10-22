// See LICENSE for license details.

package chisel3.internal.firrtl

import chisel3._
import core._
import chisel3.internal._
import chisel3.internal.sourceinfo.{SourceInfo, NoSourceInfo}

import _root_.firrtl.annotations.Annotation

case class PrimOp(val name: String) {
  override def toString: String = name
}

object PrimOp {
  val AddOp = PrimOp("add")
  val SubOp = PrimOp("sub")
  val TailOp = PrimOp("tail")
  val HeadOp = PrimOp("head")
  val TimesOp = PrimOp("mul")
  val DivideOp = PrimOp("div")
  val RemOp = PrimOp("rem")
  val ShiftLeftOp = PrimOp("shl")
  val ShiftRightOp = PrimOp("shr")
  val DynamicShiftLeftOp = PrimOp("dshl")
  val DynamicShiftRightOp = PrimOp("dshr")
  val BitAndOp = PrimOp("and")
  val BitOrOp = PrimOp("or")
  val BitXorOp = PrimOp("xor")
  val BitNotOp = PrimOp("not")
  val ConcatOp = PrimOp("cat")
  val BitsExtractOp = PrimOp("bits")
  val LessOp = PrimOp("lt")
  val LessEqOp = PrimOp("leq")
  val GreaterOp = PrimOp("gt")
  val GreaterEqOp = PrimOp("geq")
  val EqualOp = PrimOp("eq")
  val PadOp = PrimOp("pad")
  val NotEqualOp = PrimOp("neq")
  val NegOp = PrimOp("neg")
  val MultiplexOp = PrimOp("mux")
  val XorReduceOp = PrimOp("xorr")
  val ConvertOp = PrimOp("cvt")
  val AsUIntOp = PrimOp("asUInt")
  val AsSIntOp = PrimOp("asSInt")
  val AsFixedPointOp = PrimOp("asFixedPoint")
  val SetBinaryPoint = PrimOp("bpset")
  val AsClockOp = PrimOp("asClock")
}


abstract class Arg {
  def fullName(ctx: Component): String = name
  def name: String
  def pprint: String
}

case class Node(id: HasId) extends Arg {
  override def fullName(ctx: Component): String = id.getRef.fullName(ctx)
  def name: String = id.getRef.name
  def pprint = s"Node(${id.getRef.pprint})"
}

abstract class LitArg(val num: BigInt, widthArg: Width) extends Arg {
  private[chisel3] def forcedWidth = widthArg.known
  private[chisel3] def width: Width = if (forcedWidth) widthArg else Width(minWidth)

  protected def minWidth: Int
  if (forcedWidth) {
    require(widthArg.get >= minWidth,
      s"The literal value ${num} was elaborated with a specified width of ${widthArg.get} bits, but at least ${minWidth} bits are required.")
  }
  def pprint = s"LitArg(${num}, ${widthArg})"
}

case class ILit(n: BigInt) extends Arg {
  def name: String = n.toString
  def pprint =  s"ILit(${n})"
}

case class ULit(n: BigInt, w: Width) extends LitArg(n, w) {
  def name: String = "UInt" + width + "(\"h0" + num.toString(16) + "\")"
  def minWidth: Int = 1 max n.bitLength

  require(n >= 0, s"UInt literal ${n} is negative")
}

case class SLit(n: BigInt, w: Width) extends LitArg(n, w) {
  def name: String = {
    val unsigned = if (n < 0) (BigInt(1) << width.get) + n else n
    s"asSInt(${ULit(unsigned, width).name})"
  }
  def minWidth: Int = 1 + n.bitLength
}

case class FPLit(n: BigInt, w: Width, binaryPoint: BinaryPoint) extends LitArg(n, w) {
  def name: String = {
    val unsigned = if (n < 0) (BigInt(1) << width.get) + n else n
    s"asFixedPoint(${ULit(unsigned, width).name}, ${binaryPoint.asInstanceOf[KnownBinaryPoint].value})"
  }
  def minWidth: Int = 1 + n.bitLength
}

case class Ref(name: String) extends Arg {
  def pprint = s"Ref(${name})"
}
case class ModuleIO(mod: Module, name: String) extends Arg {
  override def fullName(ctx: Component): String =
    if (mod eq ctx.id) name else s"${mod.getRef.name}.$name"
  def pprint = s"ModuleIO(${mod.name}, ${name})"
}
case class Slot(imm: Node, name: String) extends Arg {
  override def fullName(ctx: Component): String =
    s"${imm.fullName(ctx)}.${name}"
  def pprint = s"Slot(${imm.pprint}, ${name})"
}
case class Index(imm: Arg, value: Arg) extends Arg {
  def name: String = s"[$value]"
  override def fullName(ctx: Component): String = s"${imm.fullName(ctx)}[${value.fullName(ctx)}]"
  def pprint = s"Index(${imm.name}, ${value.name})"
}
case class BindIndex(imm: Arg) extends Arg {
  def name: String = s"[_]"
  override def fullName(ctx: Component): String = s"${imm.fullName(ctx)}[_]"
  def pprint = s"Index(${imm.name}, _)"
}

sealed trait Bound
sealed trait NumericBound[T] extends Bound {
  val value: T
}
sealed case class Open[T](value: T) extends NumericBound[T]
sealed case class Closed[T](value: T) extends NumericBound[T]

sealed trait Range {
  val min: Bound
  val max: Bound
  def getWidth: Width
}

sealed trait KnownIntRange extends Range {
  val min: NumericBound[Int]
  val max: NumericBound[Int]

  require( (min, max) match {
    case (Open(low_val), Open(high_val)) => low_val < high_val - 1
    case (Closed(low_val), Open(high_val)) => low_val < high_val
    case (Open(low_val), Closed(high_val)) => low_val < high_val
    case (Closed(low_val), Closed(high_val)) => low_val <= high_val
  })
}

sealed case class KnownUIntRange(min: NumericBound[Int], max: NumericBound[Int]) extends KnownIntRange {
  require (min.value >= 0)

  def getWidth: Width = max match {
    case Open(v) => Width(BigInt(v - 1).bitLength.max(1))
    case Closed(v) => Width(BigInt(v).bitLength.max(1))
  }
}

sealed case class KnownSIntRange(min: NumericBound[Int], max: NumericBound[Int]) extends KnownIntRange {

  val maxWidth = max match {
    case Open(v) => Width(BigInt(v - 1).bitLength + 1)
    case Closed(v) => Width(BigInt(v).bitLength + 1)
  }
  val minWidth = min match {
    case Open(v) => Width(BigInt(v + 1).bitLength + 1)
    case Closed(v) => Width(BigInt(v).bitLength + 1)
  }
  def getWidth: Width = maxWidth.max(minWidth)

}

object Width {
  def apply(x: Int): Width = KnownWidth(x)
  def apply(): Width = UnknownWidth()
}

sealed abstract class Width {
  type W = Int
  def max(that: Width): Width = this.op(that, _ max _)
  def + (that: Width): Width = this.op(that, _ + _)
  def + (that: Int): Width = this.op(this, (a, b) => a + that)
  def shiftRight(that: Int): Width = this.op(this, (a, b) => 0 max (a - that))
  def dynamicShiftLeft(that: Width): Width =
    this.op(that, (a, b) => a + (1 << b) - 1)

  def known: Boolean
  def get: W
  protected def op(that: Width, f: (W, W) => W): Width
}

sealed case class UnknownWidth() extends Width {
  def known: Boolean = false
  def get: Int = None.get
  def op(that: Width, f: (W, W) => W): Width = this
  override def toString: String = ""
}

sealed case class KnownWidth(value: Int) extends Width {
  require(value >= 0)
  def known: Boolean = true
  def get: Int = value
  def op(that: Width, f: (W, W) => W): Width = that match {
    case KnownWidth(x) => KnownWidth(f(value, x))
    case _ => that
  }
  override def toString: String = s"<${value.toString}>"
}

object BinaryPoint {
  def apply(x: Int): BinaryPoint = KnownBinaryPoint(x)
  def apply(): BinaryPoint = UnknownBinaryPoint
}

sealed abstract class BinaryPoint {
  type W = Int
  def max(that: BinaryPoint): BinaryPoint = this.op(that, _ max _)
  def + (that: BinaryPoint): BinaryPoint = this.op(that, _ + _)
  def + (that: Int): BinaryPoint = this.op(this, (a, b) => a + that)
  def shiftRight(that: Int): BinaryPoint = this.op(this, (a, b) => 0 max (a - that))
  def dynamicShiftLeft(that: BinaryPoint): BinaryPoint =
    this.op(that, (a, b) => a + (1 << b) - 1)

  def known: Boolean
  def get: W
  protected def op(that: BinaryPoint, f: (W, W) => W): BinaryPoint
}

case object UnknownBinaryPoint extends BinaryPoint {
  def known: Boolean = false
  def get: Int = None.get
  def op(that: BinaryPoint, f: (W, W) => W): BinaryPoint = this
  override def toString: String = ""
}

sealed case class KnownBinaryPoint(value: Int) extends BinaryPoint {
  def known: Boolean = true
  def get: Int = value
  def op(that: BinaryPoint, f: (W, W) => W): BinaryPoint = that match {
    case KnownBinaryPoint(x) => KnownBinaryPoint(f(value, x))
    case _ => that
  }
  override def toString: String = s"<<${value.toString}>>"
}


sealed abstract class MemPortDirection(name: String) {
  override def toString: String = name
}
object MemPortDirection {
  object READ extends MemPortDirection("read")
  object WRITE extends MemPortDirection("write")
  object RDWR extends MemPortDirection("rdwr")
  object INFER extends MemPortDirection("infer")
}

abstract class Command {
  def sourceInfo: SourceInfo
}
abstract class Definition extends Command {
  def id: HasId
  def name: String = id.getRef.name
}
case class DefPrim[T <: Data](sourceInfo: SourceInfo, id: T, op: PrimOp, args: Arg*) extends Definition
case class DefDeclass[T <: Data](sourceInfo: SourceInfo, id: T, arg: Arg, lbl: Label) extends Definition
case class DefEndorse[T <: Data](sourceInfo: SourceInfo, id: T, arg: Arg, lbl: Label) extends Definition
case class DefNext[T <: Data](sourceInfo: SourceInfo, id: T, arg: Arg) extends Definition
case class DefInvalid(sourceInfo: SourceInfo, arg: Arg) extends Command
case class DefWire(sourceInfo: SourceInfo, id: Data, lbl: Label) extends Definition
case class DefReg(sourceInfo: SourceInfo, id: Data, clock: Arg, lbl: Label) extends Definition
case class DefRegInit(sourceInfo: SourceInfo, id: Data, clock: Arg, reset: Arg, init: Arg, lbl: Label) extends Definition
case class DefMemory(sourceInfo: SourceInfo, id: HasId, t: Data, lbl: Label, size: Int) extends Definition
case class DefSeqMemory(sourceInfo: SourceInfo, id: HasId, t: Data, lbl: Label, size: Int) extends Definition
case class DefMemPort[T <: Data](sourceInfo: SourceInfo, id: T, source: Node, dir: MemPortDirection, index: Arg, clock: Arg) extends Definition
case class DefInstance(sourceInfo: SourceInfo, id: Module, ports: Seq[Port]) extends Definition
case class WhenBegin(sourceInfo: SourceInfo, pred: Arg) extends Command
case class WhenEnd(sourceInfo: SourceInfo) extends Command
case class Connect(sourceInfo: SourceInfo, loc: Node, exp: Arg) extends Command
case class BulkConnect(sourceInfo: SourceInfo, loc1: Node, loc2: Node) extends Command
case class ConnectInit(sourceInfo: SourceInfo, loc: Node, exp: Arg) extends Command
case class Stop(sourceInfo: SourceInfo, clock: Arg, ret: Int) extends Command
case class Port(id: Data, dir: Direction)
case class Printf(sourceInfo: SourceInfo, clock: Arg, pable: Printable) extends Command
abstract class Component extends Arg {
  def id: Module
  def name: String
  def ports: Seq[Port]
  def pprint = s"Component(${id.name}, ${name}, ...)" 
}
case class DefModule(id: Module, name: String, ports: Seq[Port], commands: Seq[Command]) extends Component
case class DefBlackBox(id: Module, name: String, ports: Seq[Port], params: Map[String, Param]) extends Component

case class Circuit(name: String, components: Seq[Component], annotations: Seq[Annotation] = Seq.empty)
