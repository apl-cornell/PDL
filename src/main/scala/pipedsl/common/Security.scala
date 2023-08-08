/* Security.scala */
package pipedsl.common

import scala.util.parsing.input.Positional

object Security {

  sealed trait Label extends Positional {
    def flowsTo(lbl: Label): Boolean
    def meet(lbl: Label): Label
    def join(lbl: Label): Label
  }

  case object LTop extends Label {
    override def flowsTo(lbl: Label): Boolean = lbl match {
      case LTop => true
      case _ => false
    }
    override def meet(lbl: Label): Label = lbl
    override def join(lbl: Label): Label = LTop
  }

  case object LBottom extends Label {
    override def flowsTo(lbl: Label): Boolean = true
    override def meet(lbl: Label): Label = LBottom
    override def join(lbl: Label): Label = lbl
  }
}
