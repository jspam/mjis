package mjis.util

import scala.collection.mutable.{Map => MutableMap}

/** String-keyed map that allows lookup by prefix */
class Trie[A](keyValues: Iterable[(String, A)]) {
  private class Node {
    var item: Option[A] = None
    val children = MutableMap[Char, Node]() // could be optimized to a linear array

    def add(key: String, value: A): Unit = key.headOption match {
      case None => item = Some(value)
      case Some(c) => children.getOrElseUpdate(c, new Node).add(key.tail, value)
    }

    def tryLookupLongestPrefix(reader: LineReader): Option[A] = children.get(reader.currentChar) match {
      case None => item
      case Some(child) =>
        reader.consume()
        child.tryLookupLongestPrefix(reader)
    }
  }

  private val root = new Node
  keyValues.foreach(kv => root.add(kv._1, kv._2))

  def tryLookupLongestPrefix(reader: LineReader): Option[A] = root.tryLookupLongestPrefix(reader)
}
