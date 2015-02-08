package mjis.opt

import firm._
import firm.nodes._
import mjis.opt.FirmExtensions._
import mjis.opt.FirmExtractors._
import scala.collection.JavaConversions._
import scala.collection.mutable

object RedundantLoadElimination extends NodeBasedOptimization() {
  def isCalloc(addr: Address) = addr.getEntity.getName == "calloc" && addr.getEntity.getOwner == Program.getGlobalType

  // yes/no/dunno
  def areAliased(ptr1: Node, ptr2: Node): Option[Boolean] = (ptr1, ptr2) match {
    case (`ptr1`, `ptr1`) => Some(true)
    // poor man's inter-block CSE
    case (SelExtr(addr1, idx1), SelExtr(addr2, idx2)) if addr1 == addr2 && idx1 == idx2 => Some(true)
    case (m1: Member, m2: Member) if m1.getPtr == m2.getPtr && m1.getEntity == m2.getEntity => Some(true)
    // return values of different calloc calls are never aliased
    case (SelExtr(proj1@ReturnOfCallExtr(addr1, _), _),
          SelExtr(proj2@ReturnOfCallExtr(addr2, _), _)) if proj1 != proj2 && isCalloc(addr1) && isCalloc(addr2) => Some(false)
    case (MemberExtr(proj1@ReturnOfCallExtr(addr1, _), _),
          MemberExtr(proj2@ReturnOfCallExtr(addr2, _), _)) if proj1 != proj2 && isCalloc(addr1) && isCalloc(addr2) => Some(false)
    case (_: Member, _: Sel) => Some(false)
    case (_: Sel, _: Member) => Some(false)
     // accesses to different fields are never aliased
    case (m1: Member, m2: Member) if m1.getEntity != m2.getEntity => Some(false)
    case _ => None
  }

  def noAliasedStore(addr: Node, start: Node, stop: Node): Boolean = {
    val loopBreaker = mutable.Set[Node]()
    def rec(node: Node): Boolean = node match {
      case `stop` => true
      case _: Proj | _: Load => rec(node.getPred(0))
      case _: Phi =>
        if (loopBreaker(node)) true
        else {
          loopBreaker += node
          node.getPreds.forall(rec)
        }
      case store: Store if RedundantLoadElimination.areAliased(addr, store.getPtr) == Some(false) => rec(store.getMem)
      case _ => false
    }
    rec(start)
  }

  def tryLookupValue(n: Node, ptr: Node): Option[Node] = {
    def rec: Node => Option[Node] = {
      case store: Store => areAliased(ptr, store.getPtr) match {
        case Some(true) => Some(store.getValue)
        case Some(false) => rec(store.getMem)
        case None => None
      }
      case div: Div => rec(div.getMem)
      case mod: Mod => rec(mod.getMem)
      case load: Load => areAliased(ptr, load.getPtr) match {
        case Some(true) => load.succProj(Load.pnRes)
        case _ => rec(load.getMem)
      }
      case proj: Proj if proj.getMode == Mode.getM => rec(proj.getPred)
      case call: Call if PureFunctionCallElimination.isPure(call) => rec(call.getMem)
      case phi: Phi if noAliasedStore(ptr, phi.getPred(1), phi) => rec(phi.getPred(0))
      case _ => None
    }
    rec(n)
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
