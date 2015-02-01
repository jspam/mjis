package mjis.util

object PowerOfTwo {
  def unapply(x: Int): Option[Int] = unapply(x.toLong)
  def unapply(x: Long): Option[Int] = {
    // everybody's favorite trick
    if (x > 0 && (x & (x-1)) == 0) {
      var ret = 0
      var x2 = x
      while (x2 != 1) {
        x2 >>= 1
        ret += 1
      }
      Some(ret)
    } else
      None
  }
}
