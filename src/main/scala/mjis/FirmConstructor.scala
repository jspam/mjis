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

    override def preVisit(method: MethodDecl) = {
      val methodType = new MethodType(Array[Type](), Array[Type]())
      val methodEntity = new Entity(P.getGlobalType, mangle(method.name), methodType)
      graph = new Graph(methodEntity, 42 /* TODO */)
      constr = new Construction(graph)
    }

    override def postVisit(method: MethodDecl, _1: Unit, bodyResult: Node) = {
      val returnNode = constr.newReturn(constr.getCurrentMem, Array[Node]())
      graph.getEndBlock.addPred(returnNode)
    }

  }

  override def findings: List[Finding] = List()
}
