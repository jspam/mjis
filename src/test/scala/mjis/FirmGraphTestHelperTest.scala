package mjis

import CompilerTestMatchers._
import firm._
import org.scalatest._

class FirmGraphTestHelperTest extends FlatSpec with Matchers with BeforeAndAfter {

  var IntType: PrimitiveType = null
  var GlobalType: ClassType = null
  var MethodTypes: Map[String, MethodType] = null

  before {
    Firm.init()
    IntType = new PrimitiveType(Mode.getIs)
    GlobalType = Program.getGlobalType
    MethodTypes = Map(
      "void()" -> new MethodType(Array[Type](), Array[Type]()),
      "int(int)" -> new MethodType(Array[Type](IntType), Array[Type](IntType)),
      "int(int,int)" -> new MethodType(Array[Type](IntType, IntType), Array[Type](IntType))
    )
  }

  after {
    Firm.finish()
  }

  "The FIRM graph test helper" should "build a graph for an empty method without errors" in {
    val methodEntity = new Entity(GlobalType, "empty_method", MethodTypes("void()"))
    FirmGraphTestHelper.buildFirmGraph(methodEntity,
      """
        |start = Start
        |mem = Proj M M, start
        |return = Return, mem
        |end = End, return
      """.stripMargin) shouldBe a [Graph]
  }

  it should "build a graph for a slightly more complex method without errors" in {
    val methodEntity = new Entity(GlobalType, "return_same_arg", MethodTypes("int(int)"))
    FirmGraphTestHelper.buildFirmGraph(methodEntity,
      """
        |start = Start
        |args = Proj T T_args, start
        |a = Proj Is Arg 0, args
        |mem = Proj M M, start
        |return = Return, mem, a
        |end = End, return
      """.stripMargin) shouldBe a[Graph]
  }

  def getAddTwoArgsProgram = {
    val methodEntity = new Entity(GlobalType, "add_two_args", MethodTypes("int(int,int)"))
    FirmGraphTestHelper.buildFirmGraph(methodEntity,
      """
        |start = Start
        |args = Proj T T_args, start
        |a = Proj Is Arg 0, args
        |b = Proj Is Arg 1, args
        |a_plus_b = Add Is, a, b
        |mem_before_return = Proj M M, start
        |return_a_plus_b = Return, mem_before_return, a_plus_b
        |end = End, return_a_plus_b
      """.stripMargin)
  }

  it should "build a graph for an even more complex method without errors" in {
    getAddTwoArgsProgram shouldBe a [Graph]
  }

  it should "check two FIRM graphs for isomorphism" in {
    val refGraph = getAddTwoArgsProgram

    val methodEntity = new Entity(GlobalType, "add_two_args", MethodTypes("int(int,int)"))
    val constructedGraph = new Graph(methodEntity, 2)
    val constr = new Construction(constructedGraph)
    val varNumA = 0
    val varNumB = 1

    val args = constructedGraph.getArgs
    val projA = constr.newProj(args, Mode.getIs, 0)
    constr.setVariable(varNumA, projA)
    val projB = constr.newProj(args, Mode.getIs, 1)
    constr.setVariable(varNumB, projB)

    val valA = constr.getVariable(varNumA, Mode.getIs)
    val valB = constr.getVariable(varNumB, Mode.getIs)
    val add = constr.newAdd(valA, valB, Mode.getIs)

    val returnNode = constr.newReturn(constr.getCurrentMem, Array(add))
    constructedGraph.getEndBlock.addPred(returnNode)
    constr.finish()

    constructedGraph should beIsomorphicTo(refGraph)
  }

  it should "detect two non-isomorphic FIRM graphs" in {
    val refGraph = getAddTwoArgsProgram

    val methodEntity = new Entity(GlobalType, "add_two_args", MethodTypes("int(int,int)"))
    val constructedGraph = new Graph(methodEntity, 2)
    val constr = new Construction(constructedGraph)
    val varNumA = 0
    val varNumB = 1

    val args = constructedGraph.getArgs
    val projA = constr.newProj(args, Mode.getIs, 1)
    constr.setVariable(varNumA, projA)
    val projB = constr.newProj(args, Mode.getIs, 0)
    constr.setVariable(varNumB, projB)

    val valA = constr.getVariable(varNumA, Mode.getIs)
    val valB = constr.getVariable(varNumB, Mode.getIs)
    val add = constr.newAdd(valA, valB, Mode.getIs)

    val returnNode = constr.newReturn(constr.getCurrentMem, Array(add))
    constructedGraph.getEndBlock.addPred(returnNode)
    constr.finish()

    constructedGraph shouldNot beIsomorphicTo(refGraph)
  }

}
