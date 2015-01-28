package mjis.util

import org.scalatest.{Matchers, FlatSpec}

class DigraphTest extends FlatSpec with Matchers {
  /*
    0 -> 1 -> 2 -> 3 -> 4 -> 5 <-> 6
         |    |    |    |
         |    |<32 <    |
         |              |
         |<     41      <
   */
  val edges = Seq(0 -> 1, 1 -> 2, 2 -> 3,   3 -> 4, 4 ->5, 5 -> 6,
                                  3 -> 32, 32 -> 2,        6 -> 5,
                              4 -> 41, 41 -> 1)
  val g = new Digraph(edges.groupBy(_._1).mapValues(_.map(_._2)))

  "findStronglyConnectedComponents" should "do its job" in {
    g.findStronglyConnectedComponents(0) shouldBe Seq(
      (0, Set(0)),
      (1, Set(1, 2, 3, 4, 32, 41)),
      (5, Set(5, 6))
    )
  }

  "getSCCTree" should "do its job" in {
    g.getSCCTree(0) shouldBe Seq(
      SCCLeaf(0),
      SCCLoop(1, Seq(
        SCCLeaf(1),
        SCCLoop(2, Seq(
          SCCLeaf(2),
          SCCLeaf(3),
          SCCLeaf(32)
        )),
        SCCLeaf(4),
        SCCLeaf(41)
      )),
      SCCLoop(5, Seq(
        SCCLeaf(5),
        SCCLeaf(6)
      ))
    )
  }

  "getCondensationGraph" should "do its job" in {
    val tree = g.getSCCTree(0)
    g.getCondensationGraph(tree).edges
      // identify by dominator node
      .map(e => (e._1.nodes.head, e._2.map(_.nodes.head))) shouldBe Map(
      0 -> Seq(1),
      1 -> Seq(5),
      5 -> Seq()
    )

    g.getCondensationGraph(tree(1).asInstanceOf[SCCLoop[Int]].tree).edges
      // identify by dominator node
      .map(e => (e._1.nodes.head, e._2.map(_.nodes.head))) shouldBe Map(
      1 -> Seq(2),
      2 -> Seq(4),
      4 -> Seq(41),
      41 -> Seq(1)
    )
  }
}
