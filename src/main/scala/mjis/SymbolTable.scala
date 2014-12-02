package mjis

import mjis.ast.Decl
import scala.collection.mutable

class SymbolTable[D <: Decl] {
  private class Scope(val parent: Option[Scope]) {
    // name -> (<declaration>, <scope of previous declaration>)
    val defs = mutable.HashMap[String, (D, Option[Scope])]()
  }

  private val index = mutable.HashMap[String, Scope]()
  private var current: Option[Scope] = None

  def lookup(name: String): Option[D] = index.get(name).flatMap(_.defs.get(name)).map(_._1)

  def insert(decl: D): Unit = {
    current.get.defs += (decl.name -> ((decl, index.get(decl.name))))
    index += (decl.name -> current.get)
  }

  def enterScope(): Unit = current = Some(new Scope(current))

  def leaveScope(): Unit = {
    current.get.defs.foreach {
      case (name, (_, Some(oldscope))) => index += (name -> oldscope)
      case (name, (_, None)) => index.remove(name)
    }
    current = current.get.parent
  }

  def inCurrentScope(name: String): Boolean = current.exists(_.defs.contains(name))
}
