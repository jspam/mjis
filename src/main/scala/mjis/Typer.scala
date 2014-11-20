package mjis

import java.io.BufferedWriter
import mjis.Typer._
import mjis.ast._
import scala.collection.mutable.ListBuffer
import scala.util.control.TailCalls._

object Typer {

  abstract class SyntaxTreeError(/* element: SyntaxTree */) extends Finding {
    def severity = Severity.ERROR
    override def pos: Position = new Position(0, 0, "") // TODO: element.position
  }

  case class VoidUsageError() extends SyntaxTreeError {
    override def msg: String = s"'void' is only valid as a method return type"
  }

  case class UnresolvedReferenceError() extends SyntaxTreeError {
    override def msg: String = s"Unresolved reference"
  }

  case class InvalidTypeError(expected: TypeDef, actual: TypeDef) extends SyntaxTreeError {
    override def msg: String = s"Invalid type: expected $expected, got $actual"
  }

  case class WrongNumberOfParametersError(expected: Int, actual: Int) extends SyntaxTreeError {
    override def msg: String = s"Wrong number of parameters: expected $expected, got $actual"
  }
}

class Typer(val input: SyntaxTree) extends AnalysisPhase[SyntaxTree] {

  private case class TypecheckException(finding: Finding) extends Exception

  override protected def getResult(): SyntaxTree = { typecheckSyntaxTree(input); input }

  private val _findings = ListBuffer.empty[Finding]
  override def findings: List[Finding] = _findings.toList

  override def dumpResult(writer: BufferedWriter): Unit = ???

  @annotation.tailrec
  final def getType(t: Expression): TypeDef = {
    t match {
      case Assignment(_, rhs) => getType(rhs)
      case NewObject(typ) => typ
      case NewArray(typ, _, additionalDims) => TypeArray(typ, additionalDims + 1)
      case r: Ref[TypedDecl] => r.decl match { /* ThisLiteral, Ident, Select, Apply */
        case None => throw new TypecheckException(new UnresolvedReferenceError)
        case Some(decl) => decl.typ
      }
      case NullLiteral => TypeBasic("null")
      case _: IntLiteral => TypeBasic("int")
      case _: BooleanLiteral => TypeBasic("boolean")
    }
  }

  private def isConvertible(from: TypeDef, to: TypeDef) = {
    if (from == TypeBasic("null")) {
      // null is convertible to every reference type
      to != TypeBasic("void") && to != TypeBasic("int") && to != TypeBasic("boolean")
    } else {
      // we don't have any subtype relations
      from == to
    }
  }
  private def assertConvertible(from: TypeDef, to: TypeDef) = {
    if (!isConvertible(from, to))
      throw new TypecheckException(new InvalidTypeError(to, from))
  }

  private def assertNotVoid(typ: TypeDef) = {
    if (typ == TypeBasic("void")) {
      throw new TypecheckException(new VoidUsageError())
    }
  }

  def typecheckSyntaxTree(t: SyntaxTree) = {
    try {
      t match {
        case e: Expression => typecheckExpression(e)
        case s: Statement => typecheckStatement(s)
        case p: Program => typecheckProgram(p)
      }
    } catch {
      case TypecheckException(error) => _findings += error
    }
  }

  private def typecheckProgram(p: Program) = {
    p.classes.foreach(typecheckClassDecl)
  }

  private def typecheckClassDecl(c: ClassDecl) = {
    c.fields.foreach(typecheckFieldDecl)
    c.methods.foreach(typecheckMethodDecl)
  }

  private def typecheckFieldDecl(f: FieldDecl) = {
    assertNotVoid(f.typ)
  }

  private def typecheckMethodDecl(m: MethodDecl) = {
    typecheckStatement(m.body)
  }

  private def typecheckStatement(s: Statement): TailRec[Unit] = {
    s match {
      case LocalVarDeclStatement(_, typ, initializer) =>
        assertNotVoid(typ)
        initializer match {
          case Some(expr) =>
            typecheckExpression(expr)
            assertConvertible(getType(expr), typ)
            done(Unit)
          case None => done(Unit)
        }
      case b: Block =>
        val it = b.statements.iterator
        def remainder(): TailRec[Unit] = {
          if (it.hasNext) {
            tailcall(typecheckStatement(it.next())).flatMap(_ => remainder())
          } else done(Unit)
        }
        remainder()
      case If(cond, ifTrue, ifFalse) =>
        typecheckExpression(cond)
        assertConvertible(getType(cond), TypeBasic("boolean"))
        tailcall(typecheckStatement(ifTrue)).flatMap(_ => tailcall(typecheckStatement(ifFalse)))
      case While(cond, body) =>
        typecheckExpression(cond)
        assertConvertible(getType(cond), TypeBasic("boolean"))
        tailcall(typecheckStatement(body))
      case ExpressionStatement(expr) =>
        tailcall(typecheckExpression(expr))
      case ReturnStatement(Some(expr)) =>
        tailcall(typecheckExpression(expr))
      case _ => done(Unit)
    }
  }

  private def typecheckExpression(e: Expression): TailRec[Unit] = {
    e match {
      case Assignment(lhs, rhs) =>
        tailcall(typecheckExpression(lhs)).flatMap(_ => {
          tailcall(typecheckExpression(rhs)).flatMap(_ => {
            assertConvertible(getType(rhs), getType(lhs))
            done(Unit)
          })
        })
      case a: Apply =>
        a.decl match {
          case None => throw new TypecheckException(new UnresolvedReferenceError)
          case Some(decl) =>
            if (a.arguments.size != decl.parameters.size) {
              throw new TypecheckException(new WrongNumberOfParametersError(decl.parameters.size, a.arguments.size))
            }
            val it = a.arguments.iterator
            def remainder(): TailRec[Unit] = {
              if (it.hasNext) {
                tailcall(typecheckExpression(it.next())).flatMap(_ => remainder())
              } else done(Unit)
            }
            remainder()
        }
      case NewArray(typ, firstDimSize, _) =>
        assertNotVoid(typ)
        assertConvertible(getType(firstDimSize), TypeBasic("int"))
        done(Unit)
      case _ => done(Unit)
    }
  }

}
