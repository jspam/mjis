package mjis

abstract class TokenData(val literal: String) {
  override def toString = literal
}

object TokenData {

  // Keyword case objects
  case object BooleanType extends TokenData("boolean")
  case object Class extends TokenData("class")
  case object Else extends TokenData("else")
  case object False extends TokenData("false")
  case object If extends TokenData("if")
  case object IntType extends TokenData("int")
  case object New extends TokenData("new")
  case object Null extends TokenData("null")
  case object Public extends TokenData("public")
  case object Return extends TokenData("return")
  case object Static extends TokenData("static")
  case object This extends TokenData("this")
  case object True extends TokenData("true")
  case object VoidType extends TokenData("void")
  case object While extends TokenData("while")

  // Non-alphanumeric Tokens
  case object Unequal extends TokenData("!=")
  case object Not extends TokenData("!")
  case object ParenOpen extends TokenData("(")
  case object ParenClosed extends TokenData(")")
  case object Mult extends TokenData("*")
  case object Plus extends TokenData("+")
  case object Comma extends TokenData(",")
  case object Minus extends TokenData("-")
  case object Dot extends TokenData(".")
  case object Divide extends TokenData("/")
  case object Semicolon extends TokenData(";")
  case object SmallerEquals extends TokenData("<=")
  case object Smaller extends TokenData("<")
  case object Equals extends TokenData("==")
  case object Assign extends TokenData("=")
  case object GreaterEquals extends TokenData(">=")
  case object Greater extends TokenData(">")
  case object Modulo extends TokenData("%")
  case object LogicalAnd extends TokenData("&&")
  case object SquareBracketOpen extends TokenData("[")
  case object SquareBracketClosed extends TokenData("]")
  case object CurlyBraceOpen extends TokenData("{")
  case object CurlyBraceClosed extends TokenData("}")
  case object LogicalOr extends TokenData("||")

  case class Identifier(override val literal: String) extends TokenData(literal)
  case class IntegerLiteral(value: String) extends TokenData(value)

  // List of unused, but recognized miniJava keywords:
  // abstract, assert, break, byte, case, catch, char, const, continue, default, double, do, enum, extends, finally,
  // final, float, for, goto, implements, import, instanceof, interface, long, native, package, private, protected,
  // short, strictfp, super, switch, synchronized, throws, throw, transient, try, volatile
  // We have to recognize these and throw an error if someone uses these as identifier.
  //
  // List of unused, but recognized miniJava operators:
  // *= ++ += -= -- /= : <<= << >>= >>>= >>> >> ? %= &= & ^= ^ ~ | |=
  case class UnusedFeature(override val literal: String) extends TokenData(literal)

}

class Token(val data: TokenData, val pos: Position) {
  override def toString = s"($pos): $data"
}
