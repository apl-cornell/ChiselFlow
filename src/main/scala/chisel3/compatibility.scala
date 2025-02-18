// See LICENSE for license details.

/** The Chisel compatibility package allows legacy users to continue using the `Chisel` (capital C) package name
  *  while moving to the more standard package naming convention `chisel3` (lowercase c).
  */

package object Chisel {     // scalastyle:ignore package.object.name
  import chisel3.internal.firrtl.Width

  import scala.language.experimental.macros
  import scala.annotation.StaticAnnotation
  import scala.annotation.compileTimeOnly

  implicit val defaultCompileOptions = chisel3.core.ExplicitCompileOptions.NotStrict
  type Direction = chisel3.core.Direction

  type Label = chisel3.core.Label
  val Label = chisel3.core.Label
  val UnknownLabel = chisel3.core.UnknownLabel
  val UnknownLabelComp = chisel3.core.UnknownLabelComp
  val Level = chisel3.core.Level
  val FunLabel = chisel3.core.FunLabel
  val HLevel = chisel3.core.HLevel
  val VLabel = chisel3.core.VLabel
  val JoinLabelComp = chisel3.core.JoinLabelComp
  val MeetLabelComp = chisel3.core.MeetLabelComp
  val Declassify = chisel3.core.Declassify
  val Endorse = chisel3.core.Endorse
  val Next = chisel3.core.Next


  // Not originally part of compatibility.scala
  val Input   = chisel3.core.Input
  val Output  = chisel3.core.Output
  
  // Components
  val C = chisel3.core.C
  val I = chisel3.core.I

  val INPUT = chisel3.core.Direction.Input
  val OUTPUT = chisel3.core.Direction.Output
  val NODIR = chisel3.core.Direction.Unspecified
  object Flipped {
    def apply[T<:Data](target: T): T = chisel3.core.Flipped[T](target)
    def apply[T<:Data](target: T, lbl: Label): T = chisel3.core.Flipped[T](target, lbl)
  }
  // TODO: Possibly move the AddDirectionToData class here?
  implicit class AddDirMethodToData[T<:Data](val target: T) extends AnyVal {
    def dir: Direction = {
      target match {
        case e: Element => e.dir
        case _ => chisel3.core.Direction.Unspecified
      }
    }
  }

  type ChiselException = chisel3.internal.ChiselException

  type Data = chisel3.core.Data
  val Wire = chisel3.core.Wire
  val Clock = chisel3.core.Clock
  type Clock = chisel3.core.Clock

  type Aggregate = chisel3.core.Aggregate
  val Vec = chisel3.core.Vec
  type Vec[T <: Data] = chisel3.core.Vec[T]
  type VecLike[T <: Data] = chisel3.core.VecLike[T]
  val MonoLabelVec = chisel3.core.MonoLabelVec
  type MonoLabelVec[T <: Data] = chisel3.core.MonoLabelVec[T]
  type Record = chisel3.core.Record
  type Bundle = chisel3.core.Bundle

  val assert = chisel3.core.assert
  val stop = chisel3.core.stop

  /** This contains literal constructor factory methods that are deprecated as of Chisel3.
    */
  trait UIntFactory extends chisel3.core.UIntFactory {
    /** Create a UInt literal with inferred width. */
    def apply(n: String): UInt = n.asUInt
    /** Create a UInt literal with fixed width. */
    def apply(n: String, width: Int): UInt = n.asUInt(width.W)

    /** Create a UInt literal with specified width. */
    def apply(value: BigInt, width: Width): UInt = value.asUInt(width)

    /** Create a UInt literal with fixed width. */
    def apply(value: BigInt, width: Int): UInt = value.asUInt(width.W)

    /** Create a UInt with a specified width - compatibility with Chisel2. */
    // NOTE: This resolves UInt(width = 32)
    def apply(dir: Option[Direction] = None, width: Int): UInt = apply(width.W)
    /** Create a UInt literal with inferred width.- compatibility with Chisel2. */
    def apply(value: BigInt): UInt = value.asUInt

    /** Create a UInt with a specified direction and width - compatibility with Chisel2. */
    def apply(dir: Direction, width: Int): UInt = apply(dir, width.W)
    /** Create a UInt with a specified direction, but unspecified width - compatibility with Chisel2. */
    def apply(dir: Direction): UInt = apply(dir, Width())
    def apply(dir: Direction, width: Width): UInt = apply(dir, width, UnknownLabel)
    def apply(dir: Direction, width: Width, lbl:Label): UInt = {
      val result = apply(width, lbl)
      dir match {
        case chisel3.core.Direction.Input => chisel3.core.Input(result, lbl)
        case chisel3.core.Direction.Output => chisel3.core.Output(result, lbl)
        case chisel3.core.Direction.Unspecified => result
      }
    }

    /** Create a UInt with a specified width */
    def width(width: Int): UInt = apply(width.W)

    /** Create a UInt port with specified width. */
    def width(width: Width): UInt = apply(width)
  }

  /** This contains literal constructor factory methods that are deprecated as of Chisel3.
    */
  trait SIntFactory extends chisel3.core.SIntFactory {
    /** Create a SInt type or port with fixed width. */
    def width(width: Int): SInt = apply(width.W)
    /** Create an SInt type with specified width. */
    def width(width: Width): SInt = apply(width)

    /** Create an SInt literal with inferred width. */
    def apply(value: BigInt): SInt = value.asSInt
    /** Create an SInt literal with fixed width. */
    def apply(value: BigInt, width: Int): SInt = value.asSInt(width.W)

    /** Create an SInt literal with specified width. */
    def apply(value: BigInt, width: Width): SInt = value.asSInt(width)

    def Lit(value: BigInt): SInt = value.asSInt
    def Lit(value: BigInt, width: Int): SInt = value.asSInt(width.W)

    /** Create a SInt with a specified width - compatibility with Chisel2. */
    def apply(dir: Option[Direction] = None, width: Int): SInt = apply(width.W)
    /** Create a SInt with a specified direction and width - compatibility with Chisel2. */
    def apply(dir: Direction, width: Int): SInt = apply(dir, width.W)
    /** Create a SInt with a specified direction, but unspecified width - compatibility with Chisel2. */
    def apply(dir: Direction): SInt = apply(dir, Width())
    def apply(dir: Direction, width: Width): SInt = {
      val result = apply(width)
      dir match {
        case chisel3.core.Direction.Input => chisel3.core.Input(result)
        case chisel3.core.Direction.Output => chisel3.core.Output(result)
        case chisel3.core.Direction.Unspecified => result
      }
    }
  }

  /** This contains literal constructor factory methods that are deprecated as of Chisel3.
    */
  trait BoolFactory extends chisel3.core.BoolFactory {
    /** Creates Bool literal.
      */
    def apply(x: Boolean): Bool = x.B

    /** Create a UInt with a specified direction and width - compatibility with Chisel2. */
    def apply(dir: Direction, lbl:Label=UnknownLabel): Bool = {
      val result = apply(lbl)
      dir match {
        case chisel3.core.Direction.Input => chisel3.core.Input(result, lbl)
        case chisel3.core.Direction.Output => chisel3.core.Output(result, lbl)
        case chisel3.core.Direction.Unspecified => result
      }
    }
  }

  type Element = chisel3.core.Element
  type Bits = chisel3.core.Bits
  object Bits extends UIntFactory
  type Num[T <: Data] = chisel3.core.Num[T]
  type UInt = chisel3.core.UInt
  object UInt extends UIntFactory
  type SInt = chisel3.core.SInt
  object SInt extends SIntFactory
  type Bool = chisel3.core.Bool
  object Bool extends BoolFactory
  val Mux = chisel3.core.Mux

  type BlackBox = chisel3.core.BlackBox

  val Mem = chisel3.core.Mem
  type MemBase[T <: Data] = chisel3.core.MemBase[T]
  type Mem[T <: Data] = chisel3.core.Mem[T]
  val SeqMem = chisel3.core.SeqMem
  type SeqMem[T <: Data] = chisel3.core.SeqMem[T]

  val Module = chisel3.core.Module
  type Module = chisel3.core.Module

  val printf = chisel3.core.printf

  val Reg = chisel3.core.Reg

  val when = chisel3.core.when
  type WhenContext = chisel3.core.WhenContext

  implicit class fromBigIntToLiteral(val x: BigInt) extends chisel3.core.fromBigIntToLiteral(x)
  implicit class fromtIntToLiteral(val x: Int) extends chisel3.core.fromIntToLiteral(x)
  implicit class fromtLongToLiteral(val x: Long) extends chisel3.core.fromLongToLiteral(x)
  implicit class fromStringToLiteral(val x: String) extends chisel3.core.fromStringToLiteral(x)
  implicit class fromBooleanToLiteral(val x: Boolean) extends chisel3.core.fromBooleanToLiteral(x)
  implicit class fromIntToWidth(val x: Int) extends chisel3.core.fromIntToWidth(x)

  type BackendCompilationUtilities = firrtl.util.BackendCompilationUtilities
  val Driver = chisel3.Driver
  val ImplicitConversions = chisel3.util.ImplicitConversions

  // Deprecated as of Chisel3
  object chiselMain {
    import java.io.File

    def apply[T <: Module](args: Array[String], gen: () => T): Unit =
      Predef.assert(false, "No more chiselMain in Chisel3")

    def run[T <: Module] (args: Array[String], gen: () => T): Unit = {
      val circuit = Driver.elaborate(gen)
      Driver.parseArgs(args)
      val output_file = new File(Driver.targetDir + "/" + circuit.name + ".fir")
      Driver.dumpFirrtl(circuit, Option(output_file))
    }
  }

  @deprecated("debug doesn't do anything in Chisel3 as no pruning happens in the frontend", "chisel3")
  object debug {  // scalastyle:ignore object.name
    def apply (arg: Data): Data = arg
  }

  // Deprecated as of Chsiel3
  @throws(classOf[Exception])
  object throwException {
    def apply(s: String, t: Throwable = null) = {
      val xcpt = new Exception(s, t)
      throw xcpt
    }
  }

  object testers {    // scalastyle:ignore object.name
    type BasicTester = chisel3.testers.BasicTester
    val TesterDriver = chisel3.testers.TesterDriver
  }


  val log2Up = chisel3.util.log2Up
  val log2Ceil = chisel3.util.log2Ceil
  val log2Down = chisel3.util.log2Down
  val log2Floor = chisel3.util.log2Floor
  val isPow2 = chisel3.util.isPow2

  val BitPat = chisel3.util.BitPat
  type BitPat = chisel3.util.BitPat

  type ArbiterIO[T <: Data] = chisel3.util.ArbiterIO[T]
  type LockingArbiterLike[T <: Data] = chisel3.util.LockingArbiterLike[T]
  type LockingRRArbiter[T <: Data] = chisel3.util.LockingRRArbiter[T]
  type LockingArbiter[T <: Data] = chisel3.util.LockingArbiter[T]
  type RRArbiter[T <: Data] = chisel3.util.RRArbiter[T]
  type Arbiter[T <: Data] = chisel3.util.Arbiter[T]

  val FillInterleaved = chisel3.util.FillInterleaved
  val PopCount = chisel3.util.PopCount
  val Fill = chisel3.util.Fill
  val Reverse = chisel3.util.Reverse

  val Cat = chisel3.util.Cat

  val Log2 = chisel3.util.Log2

  val unless = chisel3.util.unless
  type SwitchContext[T <: Bits] = chisel3.util.SwitchContext[T]
  val is = chisel3.util.is
  val switch = chisel3.util.switch

  type Counter = chisel3.util.Counter
  val Counter = chisel3.util.Counter

  type DecoupledIO[+T <: Data] = chisel3.util.DecoupledIO[T]
  val DecoupledIO = chisel3.util.Decoupled
  val Decoupled = chisel3.util.Decoupled
  type QueueIO[T <: Data] = chisel3.util.QueueIO[T]
  type Queue[T <: Data] = chisel3.util.Queue[T]
  val Queue = chisel3.util.Queue

  object Enum extends chisel3.util.Enum {
    /** Returns n unique values of the specified type. Can be used with unpacking to define enums.
      *
      * nodeType must be of UInt type (note that Bits() creates a UInt) with unspecified width.
      *
      * @example {{{
      * val state_on :: state_off :: Nil = Enum(UInt(), 2)
      * val current_state = UInt()
      * switch (current_state) {
      *   is (state_on) {
      *      ...
      *   }
      *   if (state_off) {
      *      ...
      *   }
      * }
      * }}}
      */
    def apply[T <: Bits](nodeType: T, n: Int): List[T] = {
      require(nodeType.isInstanceOf[UInt], "Only UInt supported for enums")
      require(!nodeType.widthKnown, "Bit width may no longer be specified for enums")
      apply(n).asInstanceOf[List[T]]
    }

    /** An old Enum API that returns a map of symbols to UInts.
      *
      * Unlike the new list-based Enum, which can be unpacked into vals that the compiler
      * understands and can check, map accesses can't be compile-time checked and typos may not be
      * caught until runtime.
      *
      * Despite being deprecated, this is not to be removed from the compatibility layer API.
      * Deprecation is only to nag users to do something safer.
      */
    @deprecated("Use list-based Enum", "not soon enough")
    def apply[T <: Bits](nodeType: T, l: Symbol *): Map[Symbol, T] = {
      require(nodeType.isInstanceOf[UInt], "Only UInt supported for enums")
      require(!nodeType.widthKnown, "Bit width may no longer be specified for enums")
      (l zip createValues(l.length)).toMap.asInstanceOf[Map[Symbol, T]]
    }

    /** An old Enum API that returns a map of symbols to UInts.
      *
      * Unlike the new list-based Enum, which can be unpacked into vals that the compiler
      * understands and can check, map accesses can't be compile-time checked and typos may not be
      * caught until runtime.
      *
      * Despite being deprecated, this is not to be removed from the compatibility layer API.
      * Deprecation is only to nag users to do something safer.
      */
    @deprecated("Use list-based Enum", "not soon enough")
    def apply[T <: Bits](nodeType: T, l: List[Symbol]): Map[Symbol, T] = {
      require(nodeType.isInstanceOf[UInt], "Only UInt supported for enums")
      require(!nodeType.widthKnown, "Bit width may no longer be specified for enums")
      (l zip createValues(l.length)).toMap.asInstanceOf[Map[Symbol, T]]
    }
  }

  val LFSR16 = chisel3.util.LFSR16

  val ListLookup = chisel3.util.ListLookup
  val Lookup = chisel3.util.Lookup

  val Mux1H = chisel3.util.Mux1H
  val PriorityMux = chisel3.util.PriorityMux
  val MuxLookup = chisel3.util.MuxLookup
  val MuxCase = chisel3.util.MuxCase

  val OHToUInt = chisel3.util.OHToUInt
  val PriorityEncoder = chisel3.util.PriorityEncoder
  val UIntToOH = chisel3.util.UIntToOH
  val PriorityEncoderOH = chisel3.util.PriorityEncoderOH

  val RegNext = chisel3.util.RegNext
  val RegInit = chisel3.util.RegInit
  val RegEnable = chisel3.util.RegEnable
  val ShiftRegister = chisel3.util.ShiftRegister

  type ValidIO[+T <: Data] = chisel3.util.Valid[T]
  val Valid = chisel3.util.Valid
  val Pipe = chisel3.util.Pipe
  type Pipe[T <: Data] = chisel3.util.Pipe[T]


  /** Package for experimental features, which may have their API changed, be removed, etc.
    *
    * Because its contents won't necessarily have the same level of stability and support as
    * non-experimental, you must explicitly import this package to use its contents.
    */
  object experimental {
    import scala.annotation.StaticAnnotation
    import scala.annotation.compileTimeOnly

    @compileTimeOnly("enable macro paradise to expand macro annotations")
    class dump extends StaticAnnotation {
      def macroTransform(annottees: Any*): Any = macro chisel3.internal.naming.DebugTransforms.dump
    }
    @compileTimeOnly("enable macro paradise to expand macro annotations")
    class treedump extends StaticAnnotation {
      def macroTransform(annottees: Any*): Any = macro chisel3.internal.naming.DebugTransforms.treedump
    }
    @compileTimeOnly("enable macro paradise to expand macro annotations")
    class chiselName extends StaticAnnotation {
      def macroTransform(annottees: Any*): Any = macro chisel3.internal.naming.NamingTransforms.chiselName
    }
  }
}
