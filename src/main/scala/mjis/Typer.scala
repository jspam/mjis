package mjis

import java.io.BufferedWriter
import mjis.Typer._
import mjis.ast._
import mjis.Builtins._
import scala.collection.mutable.ListBuffer
import scala.util.control.TailCalls._

object Typer {

  case class TypecheckException(finding: Finding) extends Exception

  case class VoidUsageError() extends SyntaxTreeError {
    override def msg: String = s"'void' is only valid as a method return type"
  }

  case class AssignmentToNonLvalueError() extends SyntaxTreeError {
    override def msg: String = s"Assignment is only possible to a parameter, variable, field or array element"
  }

  case class ArrayAccessOnNonArrayError(actual: TypeDef) extends SyntaxTreeError {
    override def msg: String = s"Invalid type: expected an array type, got $actual"
  }

  case class UnresolvedReferenceError() extends SyntaxTreeError {
    override def msg: String = s"Unresolved reference"
  }

  case class IncomparableTypesError(type1: TypeDef, type2: TypeDef) extends SyntaxTreeError {
    override def msg: String = s"Incomparable types: $type1 and $type2"
  }

  case class InvalidTypeError(expected: TypeDef, actual: TypeDef) extends SyntaxTreeError {
    override def msg: String = s"Invalid type: expected $expected, got $actual"
  }

  case class WrongNumberOfParametersError(expected: Int, actual: Int) extends SyntaxTreeError {
    override def msg: String = s"Wrong number of parameters: expected $expected, got $actual"
  }

  case class MissingReturnStatementError() extends SyntaxTreeError {
    override def msg: String = s"Control flow may reach end of non-void function"
  }

  def getType(t: Expression) = getTypeRec(t).result
  def getTypeRec(t: Expression): TailRec[TypeDef] = {
    t match {
      case Assignment(_, rhs) => tailcall(getTypeRec(rhs))
      case NewObject(typ) => done(typ)
      case NewArray(typ, _, additionalDims) => done(TypeArray(typ, additionalDims + 1))
      case a: Apply => a.decl match {
        case None => throw new TypecheckException(new UnresolvedReferenceError)
        case Some(ArrayAccessDecl) =>
          tailcall(getTypeRec(a.arguments(0))).flatMap {
            case TypeArray(basicType, numDimensions) =>
              done(if (numDimensions == 1) basicType else TypeArray(basicType, numDimensions - 1))
            case otherType => throw new TypecheckException(new ArrayAccessOnNonArrayError(otherType))
          }
        case Some(decl) => done(decl.typ)
      }
      case r: Ref[TypedDecl] => r.decl match { /* ThisLiteral, Ident, Select */
        case None => throw new TypecheckException(new UnresolvedReferenceError)
        case Some(decl) => done(decl.typ)
      }
      case NullLiteral => done(NullType)
      case _: IntLiteral => done(IntType)
      case _: BooleanLiteral => done(BooleanType)
    }
  }
}

class Typer(val input: Program) extends AnalysisPhase[Program] {

  override protected def getResult(): Program = { typecheckProgram(input); input }

  private val _findings = ListBuffer.empty[Finding]
  override def findings: List[Finding] = _findings.toList

  override def dumpResult(writer: BufferedWriter): Unit = {} // no stdout output, just the error code

  private def isConvertible(from: TypeDef, to: TypeDef) = {
    if (from == NullType) {
      // null is convertible to every reference type
      to != VoidType && !ValueTypes.contains(to)
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
    if (typ == VoidType) {
      throw new TypecheckException(new VoidUsageError())
    }
  }

  private def isLvalue(expr: Expression) = expr match {
    case a: Apply => a.decl match {
      case Some(decl) => decl == ArrayAccessDecl
      case None => throw new TypecheckException(UnresolvedReferenceError())
    }
    case r: Ref[Decl] => r.decl match {
      case Some(decl) => decl match {
        case _: Parameter | _: FieldDecl | _: LocalVarDeclStatement => true
        case _ => false
      }
      case None => throw new TypecheckException(UnresolvedReferenceError())
    }
    case _ => false
  }

  private def typecheckProgram(p: Program) = {
    try {
      p.classes.foreach(typecheckClassDecl)
    } catch {
      case TypecheckException(error) => _findings += error
    }
  }

  private def typecheckClassDecl(c: ClassDecl) = {
    c.fields.foreach(typecheckFieldDecl)
    c.methods.foreach(typecheckMethodDecl)
  }

  private def typecheckFieldDecl(f: FieldDecl) = {
    assertNotVoid(f.typ)
  }

  private def typecheckMethodDecl(m: MethodDecl) = {
    m.parameters.foreach(p => assertNotVoid(p.typ))
    val hasReturnStatement = typecheckStatement(m.body, m).result
    if (!hasReturnStatement && m.typ != VoidType) {
      throw new TypecheckException(MissingReturnStatementError())
    }
  }

  /** @param m The surrounding method declaration of the statement
    * @return whether this statement or all of its children is/contains a ReturnStatement */
  private def typecheckStatement(s: Statement, m: MethodDecl): TailRec[Boolean] = {
    s match {
      case LocalVarDeclStatement(_, typ, initializer) =>
        assertNotVoid(typ)
        initializer match {
          case Some(expr) =>
            typecheckExpression(expr).result
            assertConvertible(getType(expr), typ)
            done(false)
          case None => done(false)
        }
      case Block(statements) =>
        def remainder(stmts: List[Statement], hasReturnStatement: Boolean): TailRec[Boolean] = stmts.headOption match {
          case None => done(hasReturnStatement)
          case Some(stmt) => tailcall(typecheckStatement(stmt, m)).
            flatMap(nextStatementHasReturn => remainder(stmts.tail, hasReturnStatement || nextStatementHasReturn))
        }
        remainder(statements, hasReturnStatement = false)
      case If(cond, ifTrue, ifFalse) =>
        typecheckExpression(cond).result
        assertConvertible(getType(cond), BooleanType)
        tailcall(typecheckStatement(ifTrue, m)).flatMap(ifTrueHasReturn => {
          tailcall(typecheckStatement(ifFalse, m)).flatMap(ifFalseHasReturn => {
            done(ifTrueHasReturn && ifFalseHasReturn)
          })
        })
      case While(cond, body) =>
        typecheckExpression(cond).result
        assertConvertible(getType(cond), BooleanType)
        tailcall(typecheckStatement(body, m))
        done(false)
      case ExpressionStatement(expr) =>
        tailcall(typecheckExpression(expr)).flatMap(_ => done(false))
      case ReturnStatement(Some(expr)) =>
        assertConvertible(getType(expr), m.typ)
        tailcall(typecheckExpression(expr)).flatMap(_ => done(true))
      case ReturnStatement(None) =>
        assertConvertible(VoidType, m.typ)
        done(true)
      case _ => done(false)
    }
  }

  private def typecheckExpression(e: Expression): TailRec[Unit] = {
    e match {
      case Assignment(lhs, rhs) =>
        if (!isLvalue(lhs)) throw new TypecheckException(AssignmentToNonLvalueError())
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
              throw new TypecheckException(new WrongNumberOfParametersError(decl.parameters.size - 1, a.arguments.size - 1))
            }
            // Special case == and !=
            if (decl == EqualsDecl || decl == UnequalDecl) {
              val typeLeft = getType(a.arguments(0))
              val typeRight = getType(a.arguments(1))
              val comparable = isConvertible(typeLeft, typeRight) || isConvertible(typeRight, typeLeft)

              if (!comparable) {
                throw new TypecheckException(new IncomparableTypesError(typeLeft, typeRight))
              }
              done(Unit)
            } else {
              def remainder(args: Seq[Expression], params: Seq[Parameter]): TailRec[Unit] =
                args.headOption match {
                  case None => done(Unit)
                  case Some(argument) => tailcall(typecheckExpression(argument)).flatMap(_ => {
                    if (!isConvertible(getType(argument), params.head.typ)) {
                      throw new TypecheckException(InvalidTypeError(params.head.typ, getType(argument)))
                    }
                    remainder(args.tail, params.tail)
                  })
              }
              if (decl == ArrayAccessDecl)
                remainder(a.arguments.tail, decl.parameters.tail)
              else
                remainder(a.arguments, decl.parameters)
            }
        }
      case NewArray(typ, firstDimSize, _) =>
        assertNotVoid(typ)
        assertConvertible(getType(firstDimSize), IntType)
        tailcall(typecheckExpression(firstDimSize)).flatMap(_ => done(Unit))
      case _ => done(Unit)
    }
  }

}
