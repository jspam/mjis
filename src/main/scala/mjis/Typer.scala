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

  case class AssignmentToNonLValueError() extends SyntaxTreeError {
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

  def getTypeForRef(r: Ref[TypedDecl]) = r.decl match {
    case None => throw new TypecheckException(new UnresolvedReferenceError)
    case Some(decl) => decl.typ
  }
  def getType(t: Expression) = getTypeRec(t).result
  def getTypeRec(t: Expression): TailRec[TypeDef] = {
    t match {
      case Assignment(lhs, _) => tailcall(getTypeRec(lhs))
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
      case r: Ref[TypedDecl] => done(getTypeForRef(r))
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

  private def isLValue(expr: Expression) = expr match {
    case a: Apply => a.decl match {
      case Some(decl) => decl == ArrayAccessDecl
      case None => throw new TypecheckException(UnresolvedReferenceError())
    }
    case r: Ref[Decl] => r.decl match {
      case Some(decl) => decl match {
        case p: Parameter => p.name != "this"
        case _: FieldDecl | _: LocalVarDeclStatement => true
        case _ => false
      }
      case None => throw new TypecheckException(UnresolvedReferenceError())
    }
    case _ => false
  }

  private def typecheckProgram(p: Program) = {
    try {
      new TyperVisitor().visit(p)
    } catch {
      case TypecheckException(error) => _findings += error
    }
  }

  private class TyperVisitor extends PlainRecursiveVisitor[Unit, Boolean, Unit]((), false, ()) {

    private var currentMethod: MethodDecl = null

    override def postVisit(f: FieldDecl, _1: Unit) = assertNotVoid(f.typ)

    override def preVisit(m: MethodDecl) = currentMethod = m

    override def postVisit(m: MethodDecl, _1: Unit, hasReturnStatement: Boolean) = {
      currentMethod = null
      if (!hasReturnStatement && m.typ != VoidType) {
        throw new TypecheckException(MissingReturnStatementError())
      }
    }

    override def postVisit(param: Parameter, _1: Unit) = assertNotVoid(param.typ)

    override def postVisit(stmt: LocalVarDeclStatement, _1: Unit, _2: Option[Unit]): Boolean = {
      assertNotVoid(stmt.typ)
      stmt.initializer match {
        case Some(expr) => assertConvertible(getType(expr), stmt.typ)
        case _ =>
      }
      false
    }

    override def postVisit(stmt: Block, haveReturnStatements: List[Boolean]): Boolean = haveReturnStatements.exists(x => x)

    override def postVisit(stmt: If, _1: Unit, ifTrueHasReturnStatement: Boolean, ifFalseHasReturnStatement: Boolean): Boolean = {
      assertConvertible(getType(stmt.condition), BooleanType)
      ifTrueHasReturnStatement && ifFalseHasReturnStatement
    }

    override def postVisit(stmt: While, _1: Unit, bodyHasReturnStatement: Boolean) = {
      assertConvertible(getType(stmt.condition), BooleanType)
      false
    }

    override def postVisit(stmt: ReturnStatement, _1: Option[Unit]) = {
      assertConvertible(stmt.returnValue.map(getType).getOrElse(VoidType), currentMethod.typ);
      true
    }

    override def postVisit(expr: Assignment, _1: Unit, _2: Unit) = {
      if (!isLValue(expr.lhs)) throw new TypecheckException(AssignmentToNonLValueError())
      assertConvertible(getType(expr.rhs), getType(expr.lhs))
    }

    override def postVisit(expr: Apply, _1: List[Unit]) = {
      expr.decl match {
        case None => throw new TypecheckException(new UnresolvedReferenceError)
        case Some(decl) =>
          if (expr.arguments.size != decl.parameters.size) {
            throw new TypecheckException(new WrongNumberOfParametersError(decl.parameters.size - 1, expr.arguments.size - 1))
          }
          // check untypeable parameters
          decl match {
            case EqualsDecl | UnequalDecl =>
              val typeLeft = getType(expr.arguments(0))
              val typeRight = getType(expr.arguments(1))
              if (!isConvertible(typeLeft, typeRight) && !isConvertible(typeRight, typeLeft)) {
                throw new TypecheckException(new IncomparableTypesError(typeLeft, typeRight))
              }
            case ArrayAccessDecl =>
              val arrayType = getType(expr.arguments(0))
              if (!arrayType.isInstanceOf[TypeArray])
                throw new TypecheckException(new ArrayAccessOnNonArrayError(arrayType))
            case _ =>
          }
          for ((arg, param) <- expr.arguments.zip(decl.parameters)) {
            // might be null, meaning that the parameter is untypeable and has already been checked above
            if (param.typ != null && !isConvertible(getType(arg), param.typ)) {
              throw new TypecheckException(InvalidTypeError(param.typ, getType(arg)))
            }
          }
      }
    }

    override def postVisit(expr: NewArray, _1: Unit, _2: Unit): Unit = {
      assertNotVoid(expr.typ)
      assertConvertible(getType(expr.firstDimSize), IntType)
    }
  }
}
