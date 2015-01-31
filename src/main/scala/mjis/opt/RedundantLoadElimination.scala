package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtractors._
import scala.collection.JavaConversions._

object RedundantLoadElimination extends NodeBasedOptimization() {
  def isCalloc(addr: Address) = addr.getEntity.getName == "calloc" && addr.getEntity.getOwner == Program.getGlobalType

  // yes/no/dunno
  def areAliased(ptr1: Node, ptr2: Node): Option[Boolean] = (ptr1, ptr2) match {
    case (`ptr1`, `ptr1`) => Some(true)
    // return values of different calloc calls are never aliased
    case (SelExtr(ReturnOfCallExtr(addr1, _), _),
          SelExtr(ReturnOfCallExtr(addr2, _), _)) if isCalloc(addr1) && isCalloc(addr2) => Some(false)
    case (_: Sel, _: Sel) => None
    case (_: Member, _: Sel) => Some(false)
    case (_: Sel, _: Member) => Some(false)
     // accesses to different fields are never aliased
    case (m1: Member, m2: Member) if m1.getEntity != m2.getEntity => Some(false)
    case _ => None
  }

  def tryLookupValue(n: Node, ptr: Node): Option[Node] = n match {
    case store: Store => areAliased(ptr, store.getPtr) match {
      case Some(true) => Some(store.getValue)
      case Some(false) => tryLookupValue(store.getMem, ptr)
      case None => None
    }
    case div: Div => tryLookupValue(div.getMem, ptr)
    case mod: Mod => tryLookupValue(mod.getMem, ptr)
    case load: Load if areAliased(ptr, load.getPtr) == Some(true) => BackEdges.getOuts(load).map(_.node).find {
      case ProjExtr(_, Load.pnRes) => true
      case _ => false
    }
    case proj: Proj if proj.getMode == Mode.getM => tryLookupValue(proj.getPred, ptr)
    case _ => None
  }

  def _optimize(g: Graph, node: Node): Unit = node match {
    case proj@ProjExtr(load: Load, Load.pnRes) =>
      tryLookupValue(load.getMem, load.getPtr).foreach { value =>
        exchange(proj, value)
        killMemoryNode(load)
      }
    case _ =>
  }
}
