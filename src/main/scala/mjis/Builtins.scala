package mjis

import mjis.ast._

object Builtins {
  val IntType = TypeBasic("int")
  val BooleanType = TypeBasic("boolean")

  val IntDecl = ClassDecl("int", List(
    MethodDecl("+", List(Parameter("", IntType), Parameter("", IntType)), IntType, null),
    MethodDecl("-", List(Parameter("", IntType), Parameter("", IntType)), IntType, null),
    MethodDecl("*", List(Parameter("", IntType), Parameter("", IntType)), IntType, null),
    MethodDecl("/", List(Parameter("", IntType), Parameter("", IntType)), IntType, null),
    MethodDecl("%", List(Parameter("", IntType), Parameter("", IntType)), IntType, null),
    MethodDecl("<", List(Parameter("", IntType), Parameter("", IntType)), BooleanType, null),
    MethodDecl("<=", List(Parameter("", IntType), Parameter("", IntType)), BooleanType, null),
    MethodDecl(">", List(Parameter("", IntType), Parameter("", IntType)), BooleanType, null),
    MethodDecl(">=", List(Parameter("", IntType), Parameter("", IntType)), BooleanType, null),
    MethodDecl("- (unary)", List(Parameter("", IntType)), IntType, null)
  ), List.empty)

  val BooleanDecl = ClassDecl("boolean", List(
    MethodDecl("||", List(Parameter("", BooleanType), Parameter("", BooleanType)), BooleanType, null),
    MethodDecl("&&", List(Parameter("", BooleanType), Parameter("", BooleanType)), BooleanType, null),
    MethodDecl("!", List(Parameter("", BooleanType)), BooleanType, null)
  ), List.empty)

  val ValueTypes = List(IntType, BooleanType)
  val ValueTypeDecls = List(IntDecl, BooleanDecl)

  val VoidType = TypeBasic("void")
  val VoidDecl = ClassDecl("void", List.empty, List.empty)

  val SystemOutPrintlnDecl = MethodDecl("System.out.println",
    List(Parameter("this", TypeBasic("$System.out")), Parameter("", IntType)), VoidType, null)
  val SystemOutFieldDecl = FieldDecl("out", TypeBasic("$System.out"))
  val SystemDecl = ClassDecl("$System", List(), List(SystemOutFieldDecl))

  val PublicTypes = ValueTypes :+ VoidType
  val PublicTypeDecls = ValueTypeDecls :+ VoidDecl

  val NullType = TypeBasic("null")

  // untypeable operators
  val EqualsDecl = MethodDecl("==",
    List(Parameter("", /* untypeable */ null), Parameter("", /* untypeable */ null)),
    BooleanType, null)
  val UnequalDecl = MethodDecl("!=",
    List(Parameter("", /* untypeable */ null), Parameter("", /* untypeable */ null)),
    BooleanType, null)
  val ArrayAccessDecl = MethodDecl("[]", List(Parameter("", /* untypeable */ null), Parameter("", IntType)),
    /* untypeable */ null, null)

  val Operators = IntDecl.methods ++ BooleanDecl.methods ++ List(EqualsDecl, UnequalDecl, ArrayAccessDecl)
}
