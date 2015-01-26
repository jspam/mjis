package mjis.util

import org.scalatest.{Matchers, FlatSpec}

class DigraphTest extends FlatSpec with Matchers {
  "findStronglyConnectedComponents" should "do its job" in {
    /*
      1 -> 2 -> 3 -> 4 -> 5 <-> 6
           |    |    |
           |<32 <    |
           |         |
           |<   42   <
     */

    val edges = Seq(1 -> 2, 2 -> 3,   3 -> 4, 4 ->5, 5 -> 6,
                            3 -> 32, 32 -> 2,        6 -> 5,
                            4 -> 42, 42 -> 2)
    val res = Digraph.findStronglyConnectedComponents(
      edges.groupBy(_._1).mapValues(_.map(_._2)),
      1
    )
    res shouldBe Seq(
      (1, Set(1)),
      (2, Set(2, 3, 4, 32, 42)),
      (5, Set(5, 6))
    )
  }
}
