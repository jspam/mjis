package mjis

import mjis.ast._

object Builtins {
  implicit val pos = Position.NoPosition
  val IntType = TypeBasic("int") /* A normal 32-bit signed integer */
  val ExtendedIntType = TypeBasic("int (including 2^31)") /* An integer that can additionally take the value 2147483648 = 2^31. */
  val BooleanType = TypeBasic("boolean")

  val IntAddDecl = MethodDecl("+", List(Parameter("", IntType), Parameter("", IntType)), IntType, null)
  val IntSubDecl = MethodDecl("-", List(Parameter("", IntType), Parameter("", IntType)), IntType, null)
  val IntMulDecl = MethodDecl("*", List(Parameter("", IntType), Parameter("", IntType)), IntType, null)
  val IntDivDecl = MethodDecl("/", List(Parameter("", IntType), Parameter("", IntType)), IntType, null)
  val IntModDecl = MethodDecl("%", List(Parameter("", IntType), Parameter("", IntType)), IntType, null)
  val IntLessDecl = MethodDecl("<", List(Parameter("", IntType), Parameter("", IntType)), BooleanType, null)
  val IntLessEqualDecl = MethodDecl("<=", List(Parameter("", IntType), Parameter("", IntType)), BooleanType, null)
  val IntGreaterDecl = MethodDecl(">", List(Parameter("", IntType), Parameter("", IntType)), BooleanType, null)
  val IntGreaterEqualDecl = MethodDecl(">=", List(Parameter("", IntType), Parameter("", IntType)), BooleanType, null)
  val IntDecl = ClassDecl("int", List(
    IntAddDecl, IntSubDecl, IntMulDecl, IntDivDecl, IntModDecl, IntLessDecl, IntLessEqualDecl, IntGreaterDecl, IntGreaterEqualDecl
  ), List.empty)

  val ExtendedIntMinusDecl = MethodDecl("- (unary)", List(Parameter("", ExtendedIntType)), IntType, null)
  val ExtendedIntDecl = ClassDecl("int (including 2^31)", List(
    ExtendedIntMinusDecl
  ), List.empty)

  val BooleanOrDecl = MethodDecl("||", List(Parameter("", BooleanType), Parameter("", BooleanType)), BooleanType, null)
  val BooleanAndDecl = MethodDecl("&&", List(Parameter("", BooleanType), Parameter("", BooleanType)), BooleanType, null)
  val BooleanNotDecl = MethodDecl("!", List(Parameter("", BooleanType)), BooleanType, null)
  val BooleanDecl = ClassDecl("boolean", List(BooleanOrDecl, BooleanAndDecl, BooleanNotDecl), List.empty)

  val ValueTypes = List(IntType, ExtendedIntType, BooleanType)
  val ValueTypeDecls = List(IntDecl, ExtendedIntDecl, BooleanDecl)

  val VoidType = TypeBasic("void")
  val VoidDecl = ClassDecl("void", List.empty, List.empty)

  val SystemOutPrintlnDecl = MethodDecl("System.out.println", List(Parameter("", IntType)), VoidType, null, isStatic = true)

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
