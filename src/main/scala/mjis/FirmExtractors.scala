package mjis

import firm._
import firm.nodes._
import scala.collection.JavaConversions._

object FirmExtractors {

  object ConstExtr {
    def unapply(node: Node): Option[Long] = node match {
      case c: Const => Some(c.getTarval.asLong())
      case _ => None
    }
  }

  object AddExtr {
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case add: Add => Some((add.getLeft, add.getRight))
      case _ => None
    }
  }

  object MulExtr {
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case mul: Mul => Some((mul.getLeft, mul.getRight))
      case _ => None
    }
  }

  object DivExtr {
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case div: Div => Some((div.getLeft, div.getRight))
      case _ => None
    }
  }

  object PhiExtr {
    def unapply(node: Node): Option[Seq[Node]] = node match {
      case phi: Phi => Some(phi.getPreds.toList)
      case _ => None
    }
  }

  object TargetValueExtr {
    def unapply(tarval: TargetValue): Option[Long] = {
      if (tarval.isConstant)
        Some(tarval.asLong())
      else
        None
    }
  }
}
