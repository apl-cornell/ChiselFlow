package chisel3.core

import chisel3._
import core._
import chisel3.internal._
import chisel3.internal.firrtl._

// Acts like a case class, but not implemented as such so that UnknownLabel can 
// inheret from it.
object Label {
  def apply(conf: LabelComp, integ: LabelComp) =
    new Label(conf, integ)
  def unapply(l: Label) = Some((l.conf, l.integ))
}

class Label private[core](val conf: LabelComp, val integ: LabelComp) {
  override def equals(that: Any) = that match {
    case lx : Label => lx.conf == this.conf && lx.integ == this.conf
    case _ => false
  }
  def name = s"{${conf.name}, ${integ.name}} "
  def fullName(ctx: Component) =
    s"{${conf.fullName(ctx)} ${integ.fullName(ctx)}} "
}

case object UnknownLabel extends Label(UnknownLabelComp, UnknownLabelComp) {
  override def name = ""
  override def fullName(ctx: Component) = ""
}

abstract class LabelComp {
  def name : String
  def fullName(ctx: Component): String
}
case object UnknownLabelComp extends LabelComp {
  def name = ""
  def fullName(ctx: Component) = ""
}
case class Level(l: String) extends LabelComp {
  def name = l
  def fullName(ctx: Component) = l
}
case class FunLabel(fn: String, id: HasId) extends LabelComp {
  def name = s"$fn ${id.getRef.name}"
  def fullName(ctx: Component) = s"$fn ${id.getRef.fullName(ctx)}"
}
case class HLevel(id: HasId) extends LabelComp {
  def name = s"[[${id.getRef.name}]]H"
  def fullName(ctx: Component) = s"[[${id.getRef.fullName(ctx)}]]H"
}

// These are not parsed by sFIRRTL and are only used internally
// case class JoinLabel(l: Label, r: Label) extends Label
// case class MeetLabel(l: Label, r: Label) extends Label
