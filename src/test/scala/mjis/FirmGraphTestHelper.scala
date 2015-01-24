package mjis

import mjis.util.FirmDumpHelper
import firm._
import firm.bindings.binding_irnode.ir_opcode
import firm.bindings.binding_ircons.op_pin_state
import firm.nodes._
import scala.collection.JavaConversions._
import scala.collection.mutable

object FirmGraphTestHelper {

  val ExpectedPrefix = "__expected_"

  def removeExpectedPrefix(name: String) =
    if (name.startsWith(ExpectedPrefix)) name.substring(ExpectedPrefix.length) else name

  def isBlock(node: Node) = node.getOpCode == ir_opcode.iro_Block

  def buildFirmGraph(methodEntity: Entity, graphDescription: String): Graph = {
    val nodes = scala.collection.mutable.Map[String, Node]()
    var curNode: Node = null

    val modes: scala.collection.immutable.Map[String, Mode] = Map(
      "b" -> Mode.getb,
      "Bu" -> Mode.getBu,
      "Is" -> Mode.getIs,
      "M" -> Mode.getM,
      "P" -> Mode.getP,
      "T" -> Mode.getT
    )
    val modeNrs: scala.collection.immutable.Map[String, Int] = Map(
      "M" -> firm.nodes.Start.pnM,
      "T_args" -> firm.nodes.Start.pnTArgs,
      "Res" -> firm.nodes.Load.pnRes,
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
        val i = "(-?\\d+)"

        val addRegex = s"Add $s".r
        val addrRegex = s"Addr $s".r
        val callRegex = s"Call $s".r
        val constRegex = s"Const $i $s".r
        val convRegex = s"Conv $s".r
        val cmpRegex = s"Cmp $s".r
        val divRegex = s"Div $s".r
        val endRegex = "End".r
        val loadRegex = s"Load $s".r
        val memberRegex = s"Member $s $i".r
        val modRegex = s"Mod $s".r
        val mulRegex = s"Mul $s".r
        val minusRegex = s"Minus $s".r
        val muxRegex = s"Mux $s".r
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
            curNode = mode match {
              case "Is" | "Bu" | "P" => constr.newConst(value.toInt, modes(mode))
              case "b" => constr.newConst(value match {
                case "true" => 1
                case "false" => 0
                case _ => throw new IllegalArgumentException(s"Invalid Boolean constant: $value")
              }, modes(mode))
            }
          case convRegex(mode) =>
            assert(args.length == 1, s"Conv needs one argument: $line")
            curNode = constr.newConv(args(0), modes(mode))
          case cmpRegex(relationType) =>
            assert(args.length == 2, s"Cmp needs two arguments: $line")
            curNode = constr.newCmp(args(0), args(1), Relation.valueOf(relationType))
          case divRegex(mode) =>
            assert(args.length == 3, s"Div needs three arguments (first one is mem): $line")
            curNode = constr.newDiv(args(0), args(1), args(2), modes(mode), op_pin_state.op_pin_state_floats)
          case endRegex() =>
            assert(args.length >= 1, s"End needs at least one argument: $line")
            args.foreach(graph.getEndBlock.addPred)
          case loadRegex(mode) =>
            assert(args.length == 2, s"Load needs two arguments (first one is memory): $line")
            curNode = constr.newLoad(args(0), args(1), modes(mode))
          case memberRegex(className, memberIndex) =>
            assert(args.length == 1, s"Member needs one argument: $line")
            val classType = Program.getTypes.find(t => t.isInstanceOf[StructType] && t.asInstanceOf[StructType].getName == className)
            assert(classType.isDefined, s"Class type $className not found")
            val memberEntity = classType.get.asInstanceOf[StructType].getMember(memberIndex.toInt)
            assert(memberEntity != null, s"Member $memberIndex in $className not found: $line")
            curNode = constr.newMember(args(0), memberEntity)
          case minusRegex(mode) =>
            assert(args.length == 1, s"Minus needs one argument: $line")
            curNode = constr.newMinus(args(0), modes(mode))
          case modRegex(mode) =>
            assert(args.length == 3, s"Mod needs three arguments (first one is mem): $line")
            curNode = constr.newMod(args(0), args(1), args(2), modes(mode), op_pin_state.op_pin_state_floats)
          case mulRegex(mode) =>
            assert(args.length == 2, s"Mul needs two arguments: $line")
            curNode = constr.newMul(args(0), args(1), modes(mode))
          case muxRegex(mode) =>
            assert(args.length == 3, s"Mux needs three arguments: $line")
            curNode = constr.newMux(args(0), args(1), args(2), modes(mode))
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
    FirmDumpHelper.dumpGraph(graph, "-FirmGraphTestHelper")
    graph
  }

  private def nodesEqual(left: Node, right: Node, visited: mutable.Set[(Node, Node)])
    : Option[String] = {
    if (left == null && right == null)
      return None
    if (left == null || right == null)
      return Some(s"$left and $right are not equal")
    
    if (visited.contains((left, right)))
      // Already checked
      return None
      
    if (left.getOpCode != right.getOpCode)
      return Some(s"Opcodes of $left and $right do not match")
    if (left.getMode != right.getMode)
      return Some(s"Modes of $left and $right do not match")

    left.getOpCode match {
      case ir_opcode.iro_Add | ir_opcode.iro_Start | ir_opcode.iro_End | ir_opcode.iro_Sub |
           ir_opcode.iro_Mul | ir_opcode.iro_Return | ir_opcode.iro_Div | ir_opcode.iro_Mod |
           ir_opcode.iro_Not | ir_opcode.iro_Call | ir_opcode.iro_Conv | ir_opcode.iro_Mux |
           ir_opcode.iro_Minus | ir_opcode.iro_Load | ir_opcode.iro_Block | ir_opcode.iro_Phi |
           ir_opcode.iro_Jmp | ir_opcode.iro_Cond | ir_opcode.iro_Store | ir_opcode.iro_Sel =>

      case ir_opcode.iro_Address =>
        val (leftAsAddr, rightAsAddr) = (new Address(left.ptr), new Address(right.ptr))
        val leftName = removeExpectedPrefix(leftAsAddr.getEntity.getLdName)
        val rightName = removeExpectedPrefix(rightAsAddr.getEntity.getLdName)
        if (leftName != rightName)
          return Some(s"Entity ldnames of $left ($leftName) and $right ($rightName) do not match")

      case ir_opcode.iro_Proj =>
        val (leftAsProj, rightAsProj) = (new Proj(left.ptr), new Proj(right.ptr))
        if (leftAsProj.getNum != rightAsProj.getNum)
          return Some (s"Projection numbers of $left (${leftAsProj.getNum}} and $right (${rightAsProj.getNum}} do not match")

      case ir_opcode.iro_Const =>
        val (leftAsConst, rightAsConst) = (new Const(left.ptr), new Const(right.ptr))
        if (leftAsConst.getTarval.compare(rightAsConst.getTarval) != Relation.Equal)
          return Some(s"Constant values of $left (${leftAsConst.getTarval}) and $right (${rightAsConst.getTarval}) do not match")

      case ir_opcode.iro_Cmp =>
        val (leftAsCmp, rightAsCmp) = (new Cmp(left.ptr), new Cmp(right.ptr))
        if (leftAsCmp.getRelation.value() != rightAsCmp.getRelation.value())
          return Some(s"Comparison modes of $left (${leftAsCmp.getRelation}) and $right (${rightAsCmp.getRelation}) do not match")

      case ir_opcode.iro_Member =>
        val (leftAsMember, rightAsMember) = (new Member(left.ptr), new Member(right.ptr))
        val leftName = removeExpectedPrefix(leftAsMember.getEntity.getLdName)
        val rightName = removeExpectedPrefix(rightAsMember.getEntity.getLdName)
        if (leftName != rightName)
          return Some(s"Entity ldnames of $left ($leftName) and $right ($rightName) do not match")

      case _ => throw new NotImplementedError(s"Unimplemented opcode: ${left.getOpCode}")
    }

    val blocksEqualError = left.getOpCode match {
      case ir_opcode.iro_Const => None // Const nodes may be placed anywhere
      case _ => nodesEqual(left.getBlock, right.getBlock, visited)
    }
    if (blocksEqualError.isDefined)
      return blocksEqualError

    if (left.getPredCount != right.getPredCount)
      return Some(s"Predecessor counts of $left and $right do not match")

    visited += ((left, right))
    left.getPreds().zip(right.getPreds()).foreach(lr => 
      nodesEqual(lr._1, lr._2, visited) match {
          case Some(s) => return Some(s + System.lineSeparator() + s"while checking predecessors of $left and $right")
          case _ =>
      })
    None
  }

  /** Returns None if the graphs are isomorphic, and an error message otherwise. */
  def isIsomorphic(left: Graph, right: Graph): Option[String] = {
    nodesEqual(left.getEndBlock, right.getEndBlock, mutable.Set())
  }

}
