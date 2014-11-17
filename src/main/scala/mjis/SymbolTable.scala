package mjis

import mjis.ast.Decl

import scala.collection.mutable

class SymbolTable {
  private class Scope(val parent: Option[Scope]) {
    val defs = mutable.HashMap[String, Decl]()

    def lookup(name: String): Option[Decl] = defs.get(name).orElse(parent.flatMap(_.lookup(name)))
  }

  private var current: Option[Scope] = None

  def lookup(name: String): Option[Decl] = current.flatMap(_.lookup(name))

  def insert(decl: Decl) = current.get.defs += (decl.name -> decl)

  def enterScope(): Unit = current = Some(new Scope(current))

  def leaveScope(): Unit = current = current.get.parent

  def inCurrentScope(name: String): Boolean = current.exists(_.defs.contains(name))
}
