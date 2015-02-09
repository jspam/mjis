package mjis.opt

import firm._
import firm.nodes._
import org.scalatest._
import mjis.opt.FirmExtensions._
import mjis.opt.FirmExtractors._
import mjis.CompilerTestHelper._

class FirmExtensionsTest extends FlatSpec with Matchers with BeforeAndAfter {
  before {
    Firm.init()
  }

  after {
    Firm.finish()
  }

  "getDominators" should "return all nodes' dominators" in {
    val g = getGraph(
      """
        |public int test(int i) {
        |  while (i > 10) i = i+1;
        |  return i;
        |}
      """.stripMargin)

    g.getEndBlock match {
      case NodeExtr(
        ret@ReturnExtr(Some(
          phi@PhiExtr(
            param: Proj,
            inc: Add
          )
        ))
      ) =>
        BackEdges.enable(g)
        val dominators = g.getDominators

        dominators(param.block) should contain only param.block
        dominators(phi.block) should contain theSameElementsAs List(param.block, phi.block)
        dominators(inc.block) should contain theSameElementsAs List(param.block, phi.block, inc.block)
        dominators(ret.block) should contain theSameElementsAs List(param.block, phi.block, ret.block)
    }
  }

  "getInductionVariables" should "find simple iteration variables" in {
    val g = getGraph(
      """
        |public int test() {
        |  int i = 5;
        |  while (i < 10) i = i+2;
        |  return i;
        |}
      """.stripMargin)

    g.getEndBlock match {
      case NodeExtr(
        ReturnExtr(Some(
          phi@PhiExtr(
            start: Const,
            incrAdd@(_ + (incr: Const))
          )
        ))
      ) =>
        BackEdges.enable(g)
        g.getInductionVariables should contain theSameElementsAs Seq(InductionVariable(phi.asInstanceOf[Phi], start, incr, incrAdd.asInstanceOf[Add]))
    }
  }

  it should "not report non-inductive variables" in {
    getGraph(
      """
        |public int test() {
        |  int i = 5;
        |  while (i < 10)
        |    if (i > 3)
        |      i = i+2;
        |  return i;
        |}
      """.stripMargin).getInductionVariables should be (empty)
  }
}
