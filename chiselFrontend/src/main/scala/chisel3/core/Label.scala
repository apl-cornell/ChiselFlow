package chisel3.core
import chisel3.internal._

abstract class Label
case object UnknownLabel extends Label
case class Level(l: String) extends Label
case class FunLabel(fn: String, id: HasId) extends Label

// These are not parsed by sFIRRTL and are only used internally
// case class JoinLabel(l: Label, r: Label) extends Label
// case class MeetLabel(l: Label, r: Label) extends Label
