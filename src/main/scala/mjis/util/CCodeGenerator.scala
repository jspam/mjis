package mjis.util

import java.io.Writer

import mjis.{Position, Builtins}
import mjis.ast._

import scala.collection.mutable

class CCodeGenerator(writer: Writer) extends PlainRecursiveVisitor[String, Unit, String]("", (), "") {
  private var indentLevel = 0
  private def indent() = indentLevel += 1
  private def dedent() = indentLevel -= 1
  private def emit(s: String) = writer.write(s)

  private def newLine(): Unit = {
    emit(System.lineSeparator())
    for (_ <- 0 until indentLevel) emit("\t")
  }

  private var methodNames = new mutable.HashMap[MethodDecl, String]()

  /* Replaces "field" by "(this)->field" if field refers to a field declaration. */
  private def wrapFieldAccess(possibleFieldAccess: Expression, exprStringToWrap: String) = possibleFieldAccess match {
    case i: Ident => i.decl match {
      case f: FieldDecl => s"(this)->$exprStringToWrap"
      case _ => exprStringToWrap
    }
    case _ => exprStringToWrap
  }

  private def methodDeclaration(method: MethodDecl) = {
    if (method.isStatic)
      "int main()"
    else {
      val paramsResult = method.parameters.map(p => s"${p.typ.accept(this)} ${p.name}").mkString(", ")
      val typ = method.returnType.accept(this)
      val name = methodNames(method)
      s"$typ $name($paramsResult)"
    }
  }

  override def preVisit(program: Program): Unit = {
    emit(s"#include <stdio.h>")
    newLine()
    emit(s"#include <stdint.h>")
    newLine()
    emit(s"#include <stdlib.h>")
    newLine()

    // Forward declare all classes and methods (except for main) and fill the method name map
    newLine()
    for (cls <- program.classes) {
      emit(s"struct ${cls.name};")
      newLine()
    }

    newLine()
    for (cls <- program.classes; m <- cls.methods) {
      methodNames += m -> (cls.name + "$" + m.name)
      if (m != program.mainMethodDecl.get) {
        emit(methodDeclaration(m) + ";")
        newLine()
      }
    }

    // Declare all classes
    for (cls <- program.classes) {newLine()
      emit(s"typedef struct ${cls.name} {")
      indent()
      cls.fields.foreach(f => { newLine(); f.accept(this) })
      dedent()
      newLine()
      emit(s"} ${cls.name};")
      newLine()
    }
  }

  override def visit(cls: ClassDecl) = {
    cls.methods.foreach(m => { newLine(); m.accept(this) })
  }

  override def preVisit(method: MethodDecl): Unit = {
    newLine()
    emit(methodDeclaration(method) + " ")
  }

  override def postVisit(field: FieldDecl, typResult: String): Unit =
    emit(s"$typResult ${field.name};")

  override def postVisit(typ: TypeBasic): String = typ match {
    case Builtins.IntType => "int32_t"
    case Builtins.BooleanType => "uint8_t"
    case Builtins.VoidType => "void"
    case _ => s"struct ${typ.name}*"
  }

  override def postVisit(typ: TypeArray, elementTypeResult: String): String =
    elementTypeResult + Seq.fill(typ.numDimensions)("*").mkString("")

  override def visit(stmt: If): Unit = {
    emit(s"if (${stmt.condition.accept(this)}) ")
    stmt.ifTrue.accept(this)
    if (!stmt.ifFalse.isEmpty) {
      emit(" else ")
      stmt.ifFalse.accept(this)
    }
  }

  override def visit(stmt: Block) = {
    emit("{")
    indent()
    stmt.statements.foreach(stmt => { newLine(); stmt.accept(this) })
    dedent()
    newLine()
    emit("}")
  }

  override def preVisit(stmt: While) =
    emit(s"while (${wrapFieldAccess(stmt.condition, stmt.condition.accept(this))}) ")

  override def postVisit(stmt: LocalVarDeclStatement, typResult: String, initializerResult: Option[String]) = {
    emit(s"$typResult ${stmt.name}")
    if (initializerResult.isDefined) emit(" = " + wrapFieldAccess(stmt.initializer.get, initializerResult.get))
    emit(";")
  }

  override def postVisit(stmt: ReturnStatement, exprResult: Option[String]) =
    emit(s"return " + wrapFieldAccess(stmt.returnValue.orNull, exprResult.getOrElse("") + ";"))

  override def postVisit(stmt: ExpressionStatement, exprResult: String) =
    emit(wrapFieldAccess(stmt.expr, exprResult) + ";")

  override def postVisit(invoc: Apply, argumentResultsNotWrapped: List[String]): String = {
    val argumentResults = invoc.arguments zip argumentResultsNotWrapped map { case (arg, argResult) => wrapFieldAccess(arg, argResult) }
    if (invoc.isOperator)
      if (argumentResults.length == 2)
        if (invoc.name == "[]")
          // array access
          s"${argumentResults(0)}[${argumentResults(1)}]"
        else
          // other binary operators
          s"(${argumentResults(0)} ${invoc.name} ${argumentResults(1)})"
      else
        // unary operators
        s"${invoc.name}(${argumentResults(0)})"
    else
      if (invoc.decl == Builtins.SystemOutPrintlnDecl)
        s"""printf("%d\\n", ${argumentResults(0)})"""
      else
        s"${methodNames(invoc.decl)}(" + argumentResults.mkString(", ") + ")"
  }

  override def postVisit(expr: Ident): String = expr.name

  override def postVisit(stmt: NullLiteral): String = "NULL"

  override def postVisit(expr: IntLiteral): String = expr.value

  override def postVisit(expr: Assignment, lhsResult: String, rhsResult: String): String =
    s"(${wrapFieldAccess(expr.lhs, lhsResult)} = ${wrapFieldAccess(expr.rhs, rhsResult)})"

  override def postVisit(expr: BooleanLiteral): String = expr match {
    case TrueLiteral() => "1"
    case FalseLiteral() => "0"
  }

  override def postVisit(expr: ThisLiteral): String = "this"

  override def postVisit(expr: NewArray, typResult: String, firstDimSizeResult: String): String = {
    implicit val pos = Position.NoPosition
    val elementType = TypeArray(expr.baseType, expr.additionalDims)
    val elementTypeResult = elementType.accept(this)
    s"(($elementTypeResult*)calloc(${wrapFieldAccess(expr.firstDimSize, firstDimSizeResult)}, sizeof($elementTypeResult)))"
  }

  override def postVisit(expr: NewObject, typResult: String): String = {
    val structType = typResult.dropRight(1) // drop trailing "*"
    s"(($typResult)calloc(1, sizeof($structType)))"
  }

  override def postVisit(expr: Select, qualifierResult: String): String =
    s"(${wrapFieldAccess(expr.qualifier, qualifierResult)})->${expr.name}"
}
