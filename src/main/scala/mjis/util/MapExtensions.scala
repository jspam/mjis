package mjis.util

import scala.collection.mutable

object MapExtensions {
  implicit class MapExtensions[A, B](map: mutable.Map[A, B]) {
    def withPersistentDefault(default: A => B) = map.withDefault(key => {
      val value = default(key)
      map += key -> value
      value
    })
  }
}
