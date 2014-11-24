package mjis

import mjis.ast._

object Builtins {
  val IntType = TypeBasic("int") /* A normal 32-bit signed integer */
  val ExtendedIntType = TypeBasic("int (including 2^31)") /* An integer that can additionally take the value 2147483648 = 2^31. */
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
    MethodDecl(">=", List(Parameter("", IntType), Parameter("", IntType)), BooleanType, null)
  ), List.empty)

  val ExtendedIntDecl = ClassDecl("int (including 2^31)", List(
    MethodDecl("- (unary)", List(Parameter("", ExtendedIntType)), IntType, null)
  ), List.empty)

  val BooleanDecl = ClassDecl("boolean", List(
    MethodDecl("||", List(Parameter("", BooleanType), Parameter("", BooleanType)), BooleanType, null),
    MethodDecl("&&", List(Parameter("", BooleanType), Parameter("", BooleanType)), BooleanType, null),
    MethodDecl("!", List(Parameter("", BooleanType)), BooleanType, null)
  ), List.empty)

  val ValueTypes = List(IntType, ExtendedIntType, BooleanType)
  val ValueTypeDecls = List(IntDecl, ExtendedIntDecl, BooleanDecl)

  val VoidType = TypeBasic("void")
  val VoidDecl = ClassDecl("void", List.empty, List.empty)

  val SystemOutPrintlnDecl = MethodDecl("System.out.println",
    List(Parameter("this", TypeBasic("$System.out")), Parameter("", IntType)), VoidType, null)
  val SystemOutFieldDecl = FieldDecl("out", TypeBasic("$System.out"))
  val SystemDecl = ClassDecl("$System", List(), List(SystemOutFieldDecl))

  val NullType = TypeBasic("null")
  val NullDecl = ClassDecl("null", List.empty, List.empty)

  val PublicTypeDecls = ValueTypeDecls ++ List(VoidDecl, NullDecl)

  // untypeable operators
  val EqualsDecl = MethodDecl("==",
    List(Parameter("", /* untypeable */ null), Parameter("", /* untypeable */ null)),
    BooleanType, null)
  val UnequalDecl = MethodDecl("!=",
    List(Parameter("", /* untypeable */ null), Parameter("", /* untypeable */ null)),
    BooleanType, null)
  val ArrayAccessDecl = MethodDecl("[]", List(Parameter("", /* untypeable */ null), Parameter("", IntType)),
    /* untypeable */ null, null)

  val Operators = IntDecl.methods ++ ExtendedIntDecl.methods ++ BooleanDecl.methods ++ List(EqualsDecl, UnequalDecl, ArrayAccessDecl)
}
