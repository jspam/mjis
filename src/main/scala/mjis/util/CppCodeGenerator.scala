package mjis.util

import java.io.Writer

import mjis.{Position, Builtins}
import mjis.ast._

class CppCodeGenerator(writer: Writer) extends PlainRecursiveVisitor[String, Unit, String]("", (), "") {
  private var indentLevel = 0
  private def indent() = indentLevel += 1
  private def dedent() = indentLevel -= 1
  private def emit(s: String) = writer.write(s)

  private def newLine(): Unit = {
    emit(System.lineSeparator())
    for (_ <- 0 until indentLevel) emit("\t")
  }

  override def preVisit(program: Program): Unit = {
    emit(s"#include <cstdio>")
    newLine()
    emit(s"#include <cstdint>")
    newLine()
  }

  override def postVisit(program: Program): Unit = {
    newLine()
    program.mainMethodDecl.get.accept(this)
    writer.flush()
  }

  override def visit(cls: ClassDecl) = {
    preVisit(cls)
    cls.fields.foreach(f => { newLine(); f.accept(this) })
    // Don't print the main method here
    cls.methods.foreach(m => if (!m.isStatic) { newLine(); m.accept(this) })
    postVisit(cls)
  }

  override def preVisit(cls: ClassDecl): Unit = {
    newLine()
    emit(s"class ${cls.name} {")
    newLine()
    emit("public:")
    indent()
    newLine()
    // Constructor with field initializers
    emit(s"${cls.name}()")
    if (cls.fields.length > 0) {
      emit(" : " + cls.fields.map(field => s"${field.name}(" + (field.typ match {
        case Builtins.IntType => "0"
        case Builtins.BooleanType => "false"
        case _ => "NULL"
      }) + ")").mkString(", "))
    }
    emit(" {}")
  }

  override def postVisit(cls: ClassDecl): Unit = {
    dedent()
    newLine()
    emit("};")
    newLine()
  }

  override def preVisit(method: MethodDecl): Unit = {
    // Do not print the this parameter
    val paramsResult =
      if (method.parameters.isEmpty) ""
      else method.parameters.tail.map(p => s"${p.typ.accept(this)} ${p.name}").mkString(", ")
    val typ = if (method.isStatic) "int" else method.returnType.accept(this)
    emit(s"$typ ${method.name}($paramsResult) ")
  }

  override def postVisit(field: FieldDecl, typResult: String): Unit =
    emit(s"$typResult ${field.name};")

  override def postVisit(typ: TypeBasic): String = typ match {
    case Builtins.IntType => "int32_t"
    case Builtins.BooleanType => "bool"
    case Builtins.VoidType => "void"
    case _ => typ.name + "*"
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
    emit(s"while (${stmt.condition.accept(this)}) ")

  override def postVisit(stmt: LocalVarDeclStatement, typResult: String, initializerResult: Option[String]) = {
    emit(s"$typResult ${stmt.name}")
    if (initializerResult.isDefined) emit(" = " + initializerResult.get)
    emit(";")
  }

  override def postVisit(stmt: ReturnStatement, exprResult: Option[String]) =
    emit(s"return " + exprResult.getOrElse("") + ";")

  override def postVisit(stmt: ExpressionStatement, exprResult: String) =
    emit(exprResult + ";")

  override def postVisit(invoc: Apply, argumentResults: List[String]): String = {
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
        s"(${argumentResults(0)})->${invoc.name}(" + argumentResults.tail.mkString(", ") + ")"
  }

  override def postVisit(expr: Ident): String = expr.name

  override def postVisit(stmt: NullLiteral): String = "NULL"

  override def postVisit(expr: IntLiteral): String = expr.value

  override def postVisit(expr: Assignment, lhsResult: String, rhsResult: String): String = s"($lhsResult = $rhsResult)"

  override def postVisit(expr: BooleanLiteral): String = expr match {
    case TrueLiteral() => "true"
    case FalseLiteral() => "false"
  }

  override def postVisit(expr: ThisLiteral): String = "this"

  override def postVisit(expr: NewArray, typResult: String, firstDimSizeResult: String): String = {
    implicit val pos = Position.NoPosition
    val elementType = TypeArray(expr.baseType, expr.additionalDims)
    val elementTypeResult = elementType.accept(this)
    s"new $elementTypeResult[$firstDimSizeResult]"
  }

  override def postVisit(expr: NewObject, typResult: String): String =
    s"new ${expr.typ.name}()"

  override def postVisit(expr: Select, qualifierResult: String): String =
    s"($qualifierResult)->${expr.name}"
}
