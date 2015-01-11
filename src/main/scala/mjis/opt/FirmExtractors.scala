package mjis.opt

import firm._
import firm.nodes._

import scala.collection.JavaConversions._

/**
 * Constant, pattern-matchable modes
 */
object Modes {
  lazy val M = Mode.getM
  lazy val Is = Mode.getIs
}

object FirmExtractors {

  object NodeExtr {
    def unapplySeq(node: Node): Option[Seq[Node]] = Some(node.getPreds.toList)
  }

  object ConstExtr {
    def unapply(node: Node): Option[Int] = node match {
      case c: Const => Some(c.getTarval.asInt())
      case _ => None
    }
  }

  object ConvExtr {
    def unapply(node: Node): Option[Node] = node match {
      case c: Conv => Some(c.getOp)
      case _ => None
    }
  }

  object ProjExtr {
    def unapply(node: Node): Option[(Node, Int)] = node match {
      case proj: Proj => Some((proj.getPred, proj.getNum))
      case _ => None
    }
  }

  object AddExtr {
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case add: Add => Some((add.getLeft, add.getRight))
      case _ => None
    }
  }

  object CondExtr {
    def unapply(node: Node): Option[Node] = node match {
      case cond: Cond => Some(cond.getSelector)
      case _ => None
    }
  }

  object SubExtr {
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case sub: Sub => Some((sub.getLeft, sub.getRight))
      case _ => None
    }
  }

  object ShlExtr {
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case shl: Shl => Some((shl.getLeft, shl.getRight))
      case _ => None
    }
  }

  object MulExtr {
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case mul: Mul => Some((mul.getLeft, mul.getRight))
      case _ => None
    }
  }

  object CmpExtr {
    def unapply(node: Node): Option[(Relation, Node, Node)] = node match {
      case cmp: Cmp => Some((cmp.getRelation, cmp.getLeft, cmp.getRight))
      case _ => None
    }
  }

  object DivExtr {
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case div: Div => Some((div.getLeft, div.getRight))
      case _ => None
    }
  }

  object ModExtr {
    def unapply(node: Node): Option[(Node, Node)] = node match {
      case mod: Mod => Some((mod.getLeft, mod.getRight))
      case _ => None
    }
  }

  object PhiExtr {
    def unapplySeq(node: Node): Option[Seq[Node]] = node match {
      case phi: Phi => Some(phi.getPreds.toList)
      case _ => None
    }
  }

  object ReturnExtr {
    def unapply(node: Node): Option[Option[Node]] = node match {
      case ret: Return => Some(
        // >= 2 because the Mem predecessor always exists
        if (ret.getPredCount >= 2)  Some(ret.getPred(1)) else None
      )
      case _ => None
    }
  }

  object CallExtr {
    /** Some(target, parameters) or None */
    def unapply(node: Node): Option[(Address, Seq[Node])] = node match {
      case call: Call => Some((call.getPtr.asInstanceOf[Address], call.getPreds.toList.drop(2) /* Mem and address */))
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
