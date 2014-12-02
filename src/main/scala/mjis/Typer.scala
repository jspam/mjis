package mjis

import java.io.BufferedWriter
import mjis.Typer._
import mjis.ast._
import mjis.Builtins._
import mjis.Position.NoPosition
import scala.collection.mutable.ListBuffer
import scala.util.control.TailCalls._

object Typer {

  case class TypecheckException(finding: Finding) extends Exception

  case class VoidUsageError(override val pos: Position) extends SyntaxTreeError {
    override def msg: String = s"'void' is only valid as a method return type"
  }

  case class AssignmentToNonLValueError(override val pos: Position) extends SyntaxTreeError {
    override def msg: String = s"Assignment is only possible to a parameter, variable, field or array element"
  }

  case class ArrayAccessOnNonArrayError(actual: TypeDef) extends SyntaxTreeError {
    override def msg: String = s"Invalid type: expected an array type, got $actual"
    override def pos: Position = actual.pos
  }

  case class UnresolvedReferenceError(override val pos: Position) extends SyntaxTreeError {
    override def msg: String = s"Unresolved reference"
  }

  case class IncomparableTypesError(type1: TypeDef, type2: TypeDef, override val pos: Position) extends SyntaxTreeError {
    override def msg: String = s"Incomparable types: $type1 and $type2"
  }

  case class IntLiteralOutOfRangeError(value: String, override val pos: Position) extends SyntaxTreeError {
    override def msg: String = s"Integer literal out of range: $value"
  }

  case class InvalidTypeError(expected: TypeDef, actual: TypeDef, override val pos: Position) extends SyntaxTreeError {
    override def msg: String = s"Invalid type: expected $expected, got $actual"
  }

  case class WrongNumberOfParametersError(expected: Int, actual: Int, override val pos: Position) extends SyntaxTreeError {
    override def msg: String = s"Wrong number of parameters: expected $expected, got $actual"
  }

  case class MissingReturnStatementError(override val pos: Position) extends SyntaxTreeError {
    override def msg: String = s"Control flow may reach end of non-void function"
  }

  case class MissingReturnValueError(override val pos: Position) extends SyntaxTreeError {
    override def msg: String = s"Non-void function must return a value"
  }

  def getTypeForRef(r: Ref[TypedDecl]) = r.decl match {
    case None => throw new TypecheckException(new UnresolvedReferenceError(r.pos))
    case Some(decl) => decl.typ
  }
  def getType(t: Expression) = getTypeRec(t).result
  def getTypeRec(t: Expression): TailRec[TypeDef] = {
    t match {
      case Assignment(lhs, _) => tailcall(getTypeRec(lhs))
      case NewObject(typ) => done(typ)
      case NewArray(typ, _, additionalDims) => done(TypeArray(typ, additionalDims + 1))
      case a: Apply => a.decl match {
        case None => throw new TypecheckException(new UnresolvedReferenceError(t.pos))
        case Some(ArrayAccessDecl) =>
          tailcall(getTypeRec(a.arguments(0))).flatMap {
            case TypeArray(basicType, numDimensions) =>
              done(if (numDimensions == 1) basicType else TypeArray(basicType, numDimensions - 1))
            case otherType => throw new TypecheckException(new ArrayAccessOnNonArrayError(otherType))
          }
        case Some(decl) => done(decl.returnType)
      }
      case r: Ref[_] => done(getTypeForRef(r.asInstanceOf[Ref[TypedDecl]]))
      case NullLiteral() => done(NullType)
      case IntLiteral(value) =>
        val MaxInt = 2147483648L
        if (value.length > MaxInt.toString.length) {
          throw new TypecheckException(new IntLiteralOutOfRangeError(value, t.pos))
        } else {
          val valueAsLong = value.toLong
          if (valueAsLong < MaxInt) done(IntType)
          else if (valueAsLong == MaxInt) done(ExtendedIntType)
          else throw new TypecheckException(new IntLiteralOutOfRangeError(value, t.pos))
        }
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
    if (from == NullType)
      // null is convertible to every reference type
      to != VoidType && !ValueTypes.contains(to)
    else if (from == IntType && to == ExtendedIntType)
      true
    else
      // we don't have any other subtype relations
      from == to
  }
  private def assertConvertible(from: TypeDef, to: TypeDef, pos: Position) = {
    if (!isConvertible(from, to))
      throw new TypecheckException(new InvalidTypeError(to, from, pos))
  }

  private def assertNotVoid(typ: TypeDef) = {
    if (typ == VoidType) {
      throw new TypecheckException(new VoidUsageError(typ.pos))
    }
  }

  private def isLValue(expr: Expression) = expr match {
    case a: Apply => a.decl match {
      case Some(decl) => decl == ArrayAccessDecl
      case None => throw new TypecheckException(UnresolvedReferenceError(expr.pos))
    }
    case r: Ref[_] => r.decl match {
      case Some(decl) => decl.isWritable
      case None => throw new TypecheckException(UnresolvedReferenceError(expr.pos))
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
      if (!hasReturnStatement && m.returnType != VoidType) {
        throw new TypecheckException(MissingReturnStatementError(m.pos))
      }
    }

    override def postVisit(t: TypeArray, _1: Unit) = assertNotVoid(t.elementType)

    override def postVisit(param: Parameter, _1: Unit) = assertNotVoid(param.typ)

    override def postVisit(stmt: LocalVarDeclStatement, _1: Unit, _2: Option[Unit]): Boolean = {
      currentMethod.numVars += 1
      assertNotVoid(stmt.typ)
      stmt.initializer match {
        case Some(expr) => assertConvertible(getType(expr), stmt.typ, expr.pos)
        case _ =>
      }
      false
    }

    override def postVisit(stmt: Block, haveReturnStatements: List[Boolean]): Boolean = haveReturnStatements.exists(x => x)

    override def postVisit(stmt: If, _1: Unit, ifTrueHasReturnStatement: Boolean, ifFalseHasReturnStatement: Boolean): Boolean = {
      assertConvertible(getType(stmt.condition), BooleanType, stmt.pos)
      ifTrueHasReturnStatement && ifFalseHasReturnStatement
    }

    override def postVisit(stmt: While, _1: Unit, bodyHasReturnStatement: Boolean) = {
      assertConvertible(getType(stmt.condition), BooleanType, stmt.pos)
      false
    }

    override def postVisit(stmt: ReturnStatement, _1: Option[Unit]) = {
      stmt.returnValue match {
        case Some(expr) =>
          assertNotVoid(getType(expr))
          assertConvertible(getType(expr), currentMethod.returnType, expr.pos)
        case None =>
          if (currentMethod.returnType != VoidType)
            throw new TypecheckException(MissingReturnValueError(stmt.pos))
      }
      true
    }

    override def postVisit(expr: Assignment, _1: Unit, _2: Unit) = {
      if (!isLValue(expr.lhs)) throw new TypecheckException(AssignmentToNonLValueError(expr.pos))
      assertConvertible(getType(expr.rhs), getType(expr.lhs), expr.pos)
      expr.lhs.isLvalue = true
    }

    override def postVisit(expr: Apply, _1: List[Unit]) = {
      expr.decl match {
        case None => throw new TypecheckException(new UnresolvedReferenceError(expr.pos))
        case Some(decl) =>
          if (expr.arguments.size != decl.parameters.size) {
            throw new TypecheckException(
              if (expr.decl.get.isStatic)
                new WrongNumberOfParametersError(decl.parameters.size, expr.arguments.size, expr.pos)
              else
                new WrongNumberOfParametersError(decl.parameters.size - 1, expr.arguments.size - 1, expr.pos)
            )
          }
          // check untypeable parameters
          decl match {
            case EqualsDecl | UnequalDecl =>
              val typeLeft = getType(expr.arguments(0))
              val typeRight = getType(expr.arguments(1))
              if (!isConvertible(typeLeft, typeRight) && !isConvertible(typeRight, typeLeft)) {
                throw new TypecheckException(new IncomparableTypesError(typeLeft, typeRight, expr.pos))
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
              throw new TypecheckException(InvalidTypeError(param.typ, getType(arg), param.pos))
            }
          }
      }
    }

    override def postVisit(expr: NewArray, _1: Unit, _2: Unit): Unit = {
      assertNotVoid(expr.typ)
      assertConvertible(getType(expr.firstDimSize), IntType, expr.pos)
    }
  }
}
