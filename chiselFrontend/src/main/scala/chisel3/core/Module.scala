// See LICENSE for license details.

package chisel3.core

import scala.collection.mutable.ArrayBuffer
import scala.language.experimental.macros
import chisel3.internal._
import chisel3.internal.Builder._
import chisel3.internal.firrtl._
import chisel3.internal.firrtl.{Command => _, _}
import chisel3.internal.sourceinfo.{InstTransform, SourceInfo, UnlocatableSourceInfo}

object Module {
  /** A wrapper method that all Module instantiations must be wrapped in
    * (necessary to help Chisel track internal state).
    *
    * @param bc the Module being created
    *
    * @return the input module `m` with Chisel metadata properly set
    */
  def apply[T <: Module](bc: => T): T = macro InstTransform.apply[T]

  def do_apply[T <: Module](bc: => T)(implicit sourceInfo: SourceInfo): T = {
    // Don't generate source info referencing parents inside a module, sincce this interferes with
    // module de-duplication in FIRRTL emission.
    val childSourceInfo = UnlocatableSourceInfo

    if (Builder.readyForModuleConstr) {
      throwException("Error: Called Module() twice without instantiating a Module." +
                     sourceInfo.makeMessage(" See " + _))
    }
    Builder.readyForModuleConstr = true
    val parent: Option[Module] = Builder.currentModule

    val m = bc.setRefs() // This will set currentModule and unset readyForModuleConstr!!!
    m._commands.prepend(DefInvalid(childSourceInfo, m.io.ref)) // init module outputs

    if (Builder.readyForModuleConstr) {
      throwException("Error: attempted to instantiate a Module, but nothing happened. " +
                     "This is probably due to rewrapping a Module instance with Module()." +
                     sourceInfo.makeMessage(" See " + _))
    }
    Builder.currentModule = parent // Back to parent!

    val ports = m.computePorts
    // Blackbox inherits from Module so we have to match on it first TODO fix
    val component = m match {
      case bb: BlackBox =>
        DefBlackBox(bb, bb.name, ports, bb.params)
      case mod: Module =>
        mod._commands.prepend(DefInvalid(childSourceInfo, mod.io.ref)) // init module outputs
        DefModule(mod, mod.name, ports, mod._commands)
    }
    m._component = Some(component)
    Builder.components += component
    // Avoid referencing 'parent' in top module
    if(!Builder.currentModule.isEmpty) {
      pushCommand(DefInstance(sourceInfo, m, ports))
      m.setupInParent(childSourceInfo)
    }
    m
  }
}

/** Abstract base class for Modules, which behave much like Verilog modules.
  * These may contain both logic and state which are written in the Module
  * body (constructor).
  *
  * @note Module instantiations must be wrapped in a Module() call.
  */
abstract class Module(
  override_clock: Option[Clock]=None, override_reset: Option[Bool]=None)
                     (implicit moduleCompileOptions: CompileOptions)
extends HasId {
  // _clock and _reset can be clock and reset in these 2ary constructors
  // once chisel2 compatibility issues are resolved
  def this(_clock: Clock)(implicit moduleCompileOptions: CompileOptions) = this(Option(_clock), None)(moduleCompileOptions)
  def this(_reset: Bool)(implicit moduleCompileOptions: CompileOptions)  = this(None, Option(_reset))(moduleCompileOptions)
  def this(_clock: Clock, _reset: Bool)(implicit moduleCompileOptions: CompileOptions) = this(Option(_clock), Option(_reset))(moduleCompileOptions)

  // This function binds the iodef as a port in the hardware graph
  private[chisel3] def Port[T<:Data](iodef: T): iodef.type = {
    // Bind each element of the iodef to being a Port
    Binding.bind(iodef, PortBinder(this), "Error: iodef")
    iodef
  }

  def annotate(annotation: ChiselAnnotation): Unit = {
    Builder.annotations += annotation
  }

  private[core] var ioDefined: Boolean = false

  /**
   * This must wrap the datatype used to set the io field of any Module.
   * i.e. All concrete modules must have defined io in this form:
   * [lazy] val io[: io type] = IO(...[: io type])
   *
   * Items in [] are optional.
   *
   * The granted iodef WILL NOT be cloned (to allow for more seamless use of
   * anonymous Bundles in the IO) and thus CANNOT have been bound to any logic.
   * This will error if any node is bound (e.g. due to logic in a Bundle
   * constructor, which is considered improper).
   *
   * TODO(twigg): Specifically walk the Data definition to call out which nodes
   * are problematic.
   */
  def IO[T<:Data](iodef: T): iodef.type = {
    require(!ioDefined, "Another IO definition for this module was already declared!")
    ioDefined = true

    Port(iodef)
  }

  // Fresh Namespace because in Firrtl, Modules namespaces are disjoint with the global namespace
  private[core] val _namespace = Namespace.empty
  private[chisel3] val _commands = ArrayBuffer[Command]()
  private[core] val _ids = ArrayBuffer[HasId]()
  Builder.currentModule = Some(this)
  if (!Builder.readyForModuleConstr) {
    throwException("Error: attempted to instantiate a Module without wrapping it in Module().")
  }
  readyForModuleConstr = false

  /** Desired name of this module. */
  def desiredName = this.getClass.getName.split('.').last

  /** Legalized name of this module. */
  final val name = Builder.globalNamespace.name(desiredName)

  /** Keep component for signal names */
  private[chisel3] var _component: Option[Component] = None

  /** Signal name (for simulation). */
  override def instanceName =
    if (_parent == None) name else _component match {
      case None => getRef.name
      case Some(c) => getRef fullName c
    }

  /** IO for this Module. At the Scala level (pre-FIRRTL transformations),
    * connections in and out of a Module may only go through `io` elements.
    */
  def io: Record
  // TODO bot is assumed to be the least-restrictive label, but it would be 
  // better to determine this from some policy
  private[core] val bot = Label(Level("L"), Level("H"))
  val clock = Port(Input(Clock(),bot))
  val reset = Port(Input(Bool(),bot))

  private[chisel3] def addId(d: HasId) { _ids += d }

  private[core] def ports: Seq[(String,Data)] = Vector(
    ("clock", clock), ("reset", reset), ("io", io)
  )

  private[core] def computePorts: Seq[firrtl.Port] = {
    // If we're auto-wrapping IO definitions, do so now.
    if (!(compileOptions.requireIOWrap || ioDefined)) {
      IO(io)
    }
    for ((name, port) <- ports) yield {
      // Port definitions need to know input or output at top-level.
      // By FIRRTL semantics, 'flipped' becomes an Input
      val direction = if(Data.isFirrtlFlipped(port)) Direction.Input else Direction.Output
      firrtl.Port(port, direction)
    }
  }

  private[core] def setupInParent(implicit sourceInfo: SourceInfo): this.type = {
    _parent match {
      case Some(p) => {
        pushCommand(DefInvalid(sourceInfo, io.ref)) // init instance inputs
        clock := override_clock.getOrElse(p.clock)
        reset := override_reset.getOrElse(p.reset)
        this
      }
      case None => this
    }
  }

  private[core] def setRefs(): this.type = {
    for ((name, port) <- ports) {
      port.setRef(ModuleIO(this, _namespace.name(name)))
    }

    /** Recursively suggests names to supported "container" classes
      * Arbitrary nestings of supported classes are allowed so long as the
      * innermost element is of type HasId
      * Currently supported:
      *   - Iterable
      *   - Option
      * (Note that Map is Iterable[Tuple2[_,_]] and thus excluded)
      */
    def nameRecursively(prefix: String, nameMe: Any): Unit =
      nameMe match {
        case (id: HasId) => id.suggestName(prefix)
        case Some(elt) => nameRecursively(prefix, elt)
        case (iter: Iterable[_]) if iter.hasDefiniteSize =>
          for ((elt, i) <- iter.zipWithIndex) {
            nameRecursively(s"${prefix}_${i}", elt)
          }
        case _ => // Do nothing
      }
    /** Scala generates names like chisel3$util$Queue$$ram for private vals
      * This extracts the part after $$ for names like this and leaves names
      * without $$ unchanged
      */
    def cleanName(name: String): String = name.split("""\$\$""").lastOption.getOrElse(name)
    for (m <- getPublicFields(classOf[Module])) {
      nameRecursively(cleanName(m.getName), m.invoke(this))
    }

    // For Module instances we haven't named, suggest the name of the Module
    _ids foreach {
      case m: Module => m.suggestName(m.desiredName)
      case _ =>
    }

    // All suggestions are in, force names to every node.
    _ids.foreach(_.forceName(default="_T", _namespace))
    
    _commands.foreach { _ match {
      case cmd:  DefMemory => 
        cmd.t.setRef(cmd.id.getRef)
      case _ =>
    }}

    _ids.foreach(_._onModuleClose)
    this
  }
  // For debuggers/testers
  lazy val getPorts = computePorts
  val compileOptions = moduleCompileOptions
}
