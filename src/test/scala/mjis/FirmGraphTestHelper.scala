package mjis

import firm._
import firm.bindings.binding_irnode.ir_opcode
import firm.bindings.binding_ircons.op_pin_state
import firm.nodes._

object FirmGraphTestHelper {

  def buildFirmGraph(methodEntity: Entity, graphDescription: String): Graph = {
    val nodes = scala.collection.mutable.Map[String, Node]()
    var curNode: Node = null

    val modes: scala.collection.immutable.Map[String, Mode] = Map(
      "b" -> Mode.getb,
      "Is" -> Mode.getIs,
      "M" -> Mode.getM,
      "T" -> Mode.getT
    )
    val modeNrs: scala.collection.immutable.Map[String, Int] = Map(
      "M" -> firm.nodes.Start.pnM,
      "T_args" -> firm.nodes.Start.pnTArgs,
      "ResDiv" -> firm.nodes.Div.pnRes,
      "ResMod" -> firm.nodes.Mod.pnRes
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
        val addrRegex = s"Addr $s".r
        val callRegex = s"Call $s".r
        val constRegex = s"Const $s $s".r
        val cmpRegex = s"Cmp $s".r
        val divRegex = s"Div $s".r
        val endRegex = "End".r
        val modRegex = s"Mod $s".r
        val mulRegex = s"Mul $s".r
        val notRegex = s"Not $s".r
        val projRegex = s"Proj $s $s".r
        val projArgRegex = s"Proj $s Arg $i".r
        val returnRegex = "Return".r
        val startRegex = "Start".r
        val subRegex = s"Sub $s".r
        definition match {
          case addRegex(mode) =>
            assert(args.length == 2, s"Add needs two arguments: $line")
            curNode = constr.newAdd(args(0), args(1), modes(mode))
          case addrRegex(name) =>
            assert(args.length == 0, s"Addr needs zero arguments: $line")
            val entity = Program.getGlobalType.getMemberByName(name)
            assert(entity != null, s"Entity $name not found: $line")
            curNode = constr.newAddress(entity)
          case callRegex(name) =>
            assert(args.length >= 2, s"Call needs at least two arguments (mem and called function address): $line")
            val entity = Program.getGlobalType.getMemberByName(name)
            assert(entity != null, s"Entity $name not found: $line")
            curNode = constr.newCall(args(0), args(1), args.drop(2), entity.getType)
          case constRegex(value, mode) =>
            assert(args.length == 0, s"Const needs zero arguments: $line")
            curNode = constr.newConst(mode match {
              case "Is" => value.toInt
              case "b" => value match {
                case "true" => 1
                case "false" => 0
                case _ => throw new IllegalArgumentException(s"Invalid Boolean constant: $value")
              }
            }, modes(mode))
          case cmpRegex(relationType) =>
            assert(args.length == 2, s"Cmp needs two arguments: $line")
            curNode = constr.newCmp(args(0), args(1), Relation.valueOf(relationType))
          case divRegex(mode) =>
            assert(args.length == 3, s"Div needs three arguments (first one is mem): $line")
            curNode = constr.newDiv(args(0), args(1), args(2), modes(mode), op_pin_state.op_pin_state_floats)
          case endRegex() =>
            assert(args.length >= 1, s"End needs at least one argument: $line")
            args.foreach(graph.getEndBlock.addPred)
          case modRegex(mode) =>
            assert(args.length == 3, s"Mod needs three arguments (first one is mem): $line")
            curNode = constr.newMod(args(0), args(1), args(2), modes(mode), op_pin_state.op_pin_state_floats)
          case mulRegex(mode) =>
            assert(args.length == 2, s"Mul needs two arguments: $line")
            curNode = constr.newMul(args(0), args(1), modes(mode))
          case notRegex(mode) =>
            assert(args.length == 1, s"Not needs one argument: $line")
            curNode = constr.newNot(args(0), modes(mode))
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
          case subRegex(mode) =>
            assert(args.length == 2, s"Sub needs two arguments: $line")
            curNode = constr.newSub(args(0), args(1), modes(mode))
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
      case ir_opcode.iro_Add | ir_opcode.iro_Start | ir_opcode.iro_End | ir_opcode.iro_Sub |
           ir_opcode.iro_Mul | ir_opcode.iro_Return | ir_opcode.iro_Div | ir_opcode.iro_Mod |
           ir_opcode.iro_Not | ir_opcode.iro_Call => ""
      case ir_opcode.iro_Address =>
        val (leftAsAddr, rightAsAddr) = (new Address(left.ptr), new Address(right.ptr))
        if (leftAsAddr.getEntity.getLdName == rightAsAddr.getEntity.getLdName) ""
        else s"Entity ldnames of $left (${leftAsAddr.getEntity.getLdName}) and $right (${rightAsAddr.getEntity.getLdName}) do not match"
      case ir_opcode.iro_Proj =>
        val (leftAsProj, rightAsProj) = (new Proj(left.ptr), new Proj(right.ptr))
        if (leftAsProj.getNum == rightAsProj.getNum) ""
        else s"Projection numbers of $left (${leftAsProj.getNum}} and $right (${rightAsProj.getNum}} do not match"
      case ir_opcode.iro_Const =>
        val (leftAsConst, rightAsConst) = (new Const(left.ptr), new Const(right.ptr))
        if (leftAsConst.getTarval.compare(rightAsConst.getTarval) == Relation.Equal) ""
        else s"Constant values of $left (${leftAsConst.getTarval}) and $right (${rightAsConst.getTarval}) do not match"
      case ir_opcode.iro_Cmp =>
        val (leftAsCmp, rightAsCmp) = (new Cmp(left.ptr), new Cmp(right.ptr))
        if (leftAsCmp.getRelation.value() == rightAsCmp.getRelation.value()) ""
        else s"Comparison modes of $left (${leftAsCmp.getRelation}) and $right (${rightAsCmp.getRelation}) do not match"
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
      return "Predecessor count of end blocks does not match " +
        s"(left: ${leftEndBlock.getPredCount}, right: ${rightEndBlock.getPredCount})"
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
