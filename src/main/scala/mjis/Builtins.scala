package mjis

import mjis.ast._

object Builtins {
  val IntType = TypeBasic("int")
  val BooleanType = TypeBasic("boolean")

  val IntDecl = ClassDecl("int", List(
    MethodDecl("+", List(Parameter("", IntType)), IntType, null),
    MethodDecl("-", List(Parameter("", IntType)), IntType, null),
    MethodDecl("*", List(Parameter("", IntType)), IntType, null),
    MethodDecl("/", List(Parameter("", IntType)), IntType, null),
    MethodDecl("%", List(Parameter("", IntType)), IntType, null),
    MethodDecl("<", List(Parameter("", IntType)), BooleanType, null),
    MethodDecl("<=", List(Parameter("", IntType)), BooleanType, null),
    MethodDecl(">", List(Parameter("", IntType)), BooleanType, null),
    MethodDecl(">=", List(Parameter("", IntType)), BooleanType, null),
    MethodDecl("- (unary)", List(), IntType, null)
  ), List.empty)

  val BooleanDecl = ClassDecl("boolean", List(
    MethodDecl("||", List(Parameter("", BooleanType)), BooleanType, null),
    MethodDecl("&&", List(Parameter("", BooleanType)), BooleanType, null),
    MethodDecl("!", List(), BooleanType, null)
  ), List.empty)

  val ValueTypes = List(IntType, BooleanType)

  val VoidType = TypeBasic("void")

  val PublicTypes = ValueTypes :+ VoidType

  val NullType = TypeBasic("null")

  // untypeable operators
  val EqualsDecl = MethodDecl("==", null, null, null)
  val UnequalDecl = MethodDecl("!=", null, null, null)
  val ArrayAccessDecl = MethodDecl("[]", null, null, null)
  val SystemOutPrintlnDecl = MethodDecl("System.out.println", List(Parameter("", IntType)), VoidType, null)
}
