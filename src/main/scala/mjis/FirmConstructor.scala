package mjis

import java.io.BufferedWriter

import firm.bindings.binding_ircons.op_pin_state
import firm.{Program => P, _}
import firm.nodes._
import mjis.ast._
import scala.collection.JavaConversions._

class FirmConstructor(input: Program) extends Phase[Unit] {
  override protected def getResult(): Unit = {
    transformProgram(input)
  }

  override def dumpResult(writer: BufferedWriter): Unit = {
    P.getGraphs.foreach(Dump.dumpGraph(_, "-FirmConstructor"))
  }

  private def transformProgram(prog: Program) = new FirmConstructorVisitor().visit(prog)

  private def mangle(methodName: String) = methodName

  private class FirmConstructorVisitor extends PlainRecursiveVisitor[Unit, Node, Node]((), null, null) {

    private var graph: Graph = null
    private var constr: Construction = null

    def getFirmType(typ: TypeDef) = typ match {
      case TypeBasic(_) => new PrimitiveType(Mode.getIs)
      case _ => ???
    }

    override def preVisit(method: MethodDecl) = {
      // For the FIRM graph, main has signature void() as args may not be accessed anyway
      val params = if (method.isStatic) Seq() else method.parameters
      val paramTypes: Array[Type] = (params map { p => getFirmType(p.typ) }).toArray
      val resultTypes: Array[Type] = method.typ match {
        case Builtins.VoidType => Array[Type]()
        case t => Array[Type](getFirmType(t))
      }
      val methodType = new MethodType(paramTypes, resultTypes)
      val methodEntity = new Entity(P.getGlobalType, mangle(method.name), methodType)
      graph = new Graph(methodEntity, method.numVars)
      constr = new Construction(graph)
    }

    override def postVisit(method: MethodDecl, _1: Unit, bodyResult: Node): Unit = {
      if (method.typ == Builtins.VoidType) {
        graph.getEndBlock.addPred(constr.newReturn(constr.getCurrentMem, Array[Node]()))
      }
    }

    override def postVisit(stmt: ReturnStatement, exprResult: Option[Node]): Node = {
      val returnNode = constr.newReturn(constr.getCurrentMem, exprResult match {
        case None => Array[Node]()
        case Some(valueNode) => Array[Node](valueNode)
      })
      graph.getEndBlock.addPred(returnNode)
      returnNode
    }

    override def postVisit(expr: BooleanLiteral): Node = {
      constr.newConst(expr match {
        case TrueLiteral => 1
        case FalseLiteral => 0
      }, Mode.getIs)
    }

    override def postVisit(expr: IntLiteral): Node = {
      constr.newConst(expr.value.toInt, Mode.getIs) // TODO: what about 2^31?
    }

    override def postVisit(expr: Apply, argumentResults: List[Node]): Node = {
      expr.decl match {
        case Some(Builtins.IntAddDecl) =>
          constr.newAdd(argumentResults(0), argumentResults(1), Mode.getIs)
        case Some(Builtins.IntSubDecl) =>
          constr.newSub(argumentResults(0), argumentResults(1), Mode.getIs)
        case Some(Builtins.IntMulDecl) =>
          constr.newMul(argumentResults(0), argumentResults(1), Mode.getIs)
        case Some(Builtins.IntDivDecl) =>
          val divNode = constr.newDiv(constr.getCurrentMem, argumentResults(0), argumentResults(1),
            Mode.getIs, op_pin_state.op_pin_state_floats)
          constr.setCurrentMem(constr.newProj(divNode, Mode.getM, firm.nodes.Div.pnM))
          constr.newProj(divNode, Mode.getIs, firm.nodes.Div.pnRes)
        case Some(Builtins.IntModDecl) =>
          val modNode = constr.newMod(constr.getCurrentMem, argumentResults(0), argumentResults(1),
            Mode.getIs, op_pin_state.op_pin_state_floats)
          constr.setCurrentMem(constr.newProj(modNode, Mode.getM, firm.nodes.Mod.pnM))
          constr.newProj(modNode, Mode.getIs, firm.nodes.Mod.pnRes)
        case _ => ???
      }
    }
  }

  override def findings: List[Finding] = List()
}
