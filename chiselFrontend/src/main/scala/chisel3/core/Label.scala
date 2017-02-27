package chisel3.core
import chisel3.internal._

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
}

case object UnknownLabel extends Label(UnknownLabelComp, UnknownLabelComp)

abstract class LabelComp
case object UnknownLabelComp extends LabelComp
case class Level(l: String) extends LabelComp
case class FunLabel(fn: String, id: HasId) extends LabelComp
case class HLevel(id: HasId) extends LabelComp

// These are not parsed by sFIRRTL and are only used internally
// case class JoinLabel(l: Label, r: Label) extends Label
// case class MeetLabel(l: Label, r: Label) extends Label
