package mjis

import java.io.BufferedWriter

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

  private class FirmConstructorVisitor extends PlainRecursiveVisitor[Unit, Node, Unit]((), null, ()) {

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
      graph = new Graph(methodEntity, 42 /* TODO */)
      constr = new Construction(graph)
    }

    override def postVisit(method: MethodDecl, _1: Unit, bodyResult: Node) = {
      val returnNode = constr.newReturn(constr.getCurrentMem, method.typ match {
        case Builtins.VoidType => Array[Node]()
        case t => Array[Node](constr.newConst(0, Mode.getIs))
      })
      graph.getEndBlock.addPred(returnNode)
    }

  }

  override def findings: List[Finding] = List()
}
