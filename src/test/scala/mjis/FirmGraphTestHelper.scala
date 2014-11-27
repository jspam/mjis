package mjis

import firm._
import firm.bindings.binding_irnode.ir_opcode
import firm.nodes._

object FirmGraphTestHelper {

  def buildFirmGraph(methodEntity: Entity, graphDescription: String): Graph = {
    val nodes = scala.collection.mutable.Map[String, Node]()
    var curNode: Node = null

    val modes: scala.collection.immutable.Map[String, Mode] = Map(
      "Is" -> Mode.getIs,
      "M" -> Mode.getM,
      "T" -> Mode.getT
    )
    val modeNrs: scala.collection.immutable.Map[String, Int] = Map(
      "M" -> firm.nodes.Start.pnM,
      "T_args" -> firm.nodes.Start.pnTArgs
    )

    val graph = new Graph(methodEntity, 42 /* TODO */)
    val constr = new Construction(graph)
    var curBlock = graph.getStartBlock

    graphDescription.split("[\\r\\n]+").foreach(line => {
      if (!line.trim.isEmpty && !line.startsWith("#")) {
        val splitLine = line.split(" = ")
        assert(splitLine.length == 2, s"Invalid syntax: '$line'")
        val nodeName = splitLine(0)
        assert(!nodes.contains(nodeName), s"Node $nodeName is already defined: '$line'")
        val definitionAndArgs = splitLine(1).split(",").map(_.trim).toList
        val definition = definitionAndArgs.head
        definitionAndArgs.tail.foreach(
          argNodeName => assert(nodes.contains(argNodeName), s"Unknown argument node '$argNodeName': '$line'")
        )
        val args = definitionAndArgs.tail.map(nodes).toArray

        val s = "(\\w+)"
        val i = "(\\d+)"

        val addRegex = s"Add $s".r
        val constRegex = s"Const $i $s".r
        val endRegex = "End".r
        val projRegex = s"Proj $s $s".r
        val projArgRegex = s"Proj $s Arg $i".r
        val returnRegex = "Return".r
        val startRegex = "Start".r
        definition match {
          case addRegex(mode) =>
            assert(args.length == 2, s"Add needs two arguments: $line")
            curNode = constr.newAdd(args(0), args(1), modes(mode))
          case constRegex(value, mode) =>
            assert(args.length == 0, s"Const needs zero arguments: $line")
            curNode = constr.newConst(value.toInt, modes(mode))
          case endRegex() =>
            assert(args.length >= 1, s"End needs at least one argument: $line")
            args.foreach(graph.getEndBlock.addPred)
          case projRegex(mode, argNr) =>
            assert(args.length == 1, s"Proj needs one argument: $line")
            curNode = constr.newProj(args(0), modes(mode), modeNrs(argNr))
          case projArgRegex(mode, argNr) =>
            assert(args.length == 1, s"Proj needs one argument: $line")
            curNode = constr.newProj(args(0), modes(mode), argNr.toInt)
          case returnRegex() =>
            assert(args.length >= 1, s"Return needs at least one argument: $line")
            curNode = constr.newReturn(args.head, args.tail)
          case startRegex() =>
            assert(args.length == 0, s"Start needs zero arguments: $line")
            curNode = graph.getStart
        }
        nodes.update(nodeName, curNode)
      } else if (line.trim.isEmpty) {
        curBlock = constr.newBlock()
      }
    })
    constr.finish()
    graph.check()
    Dump.dumpGraph(graph, "-FirmGraphTestHelper")
    graph
  }

  private def nodesEqual(left: Node, right: Node): String = {
    if (left.getOpCode != right.getOpCode)
      return s"Opcodes of $left and $right do not match"
    if (left.getMode != right.getMode)
      return s"Modes of $left and $right do not match"
    left.getOpCode match {
      case ir_opcode.iro_Add | ir_opcode.iro_Start | ir_opcode.iro_End | ir_opcode.iro_Return => ""
      case ir_opcode.iro_Proj =>
        val leftAsProj = new Proj(left.ptr)
        val rightAsProj = new Proj(right.ptr)
        if (leftAsProj.getNum == rightAsProj.getNum) "" else s"Projection numbers of $left and $right do not match"
      case ir_opcode.iro_Const =>
        val leftAsConst = new Const(left.ptr)
        val rightAsConst = new Const(right.ptr)
        if (leftAsConst.getTarval.compare(rightAsConst.getTarval) == Relation.Equal) ""
        else s"Constant values of $left and $right do not match"
      case _ => throw new NotImplementedError(s"Unimplemented opcode: ${left.getOpCode}")
    }
  }

  /** Returns an empty string if the graphs are isomorphic, and an error message otherwise. */
  def isIsomorphic(left: Graph, right: Graph): String = {
    val leftVisited = scala.collection.mutable.Set[Int]()
    val rightVisited = scala.collection.mutable.Set[Int]()
    val workList = scala.collection.mutable.ListBuffer[(Node, Node)]()

    val leftEndBlock = left.getEndBlock
    val rightEndBlock = right.getEndBlock

    if (leftEndBlock.getPredCount != rightEndBlock.getPredCount)
      return "Predecessor count of end blocks does not match"
    for (i <- 0 until leftEndBlock.getPredCount) workList.+=((leftEndBlock.getPred(i), rightEndBlock.getPred(i)))

    while (workList.nonEmpty) {
      val (leftNode, rightNode) = workList.remove(0)
      nodesEqual(leftNode, rightNode) match {
        case "" =>
        case s => return s
      }
      if (leftNode.getPredCount != rightNode.getPredCount)
        return s"Predecessor counts of $leftNode and $rightNode do not match"
      for (i <- 0 until leftNode.getPredCount) {
        val leftPred = leftNode.getPred(i)
        val rightPred = rightNode.getPred(i)

        if (leftVisited(leftPred.getNr)) {
          if (!rightVisited(rightPred.getNr))
            return s"$leftPred was already visited, but $rightPred was not"
        } else if (rightVisited(rightPred.getNr)) {
          if (!leftVisited(leftPred.getNr))
            return s"$rightPred was already visited, but $leftPred was not"
        } else {
          leftVisited.add(leftPred.getNr)
          rightVisited.add(rightPred.getNr)
          workList.+=((leftPred, rightPred))
        }
      }
    }

    ""
  }

}
