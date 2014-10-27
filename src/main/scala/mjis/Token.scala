package mjis

abstract class TokenData {}

object TokenData {

  // Keyword case objects
  case object BooleanType extends TokenData
  case object Class extends TokenData
  case object Else extends TokenData
  case object False extends TokenData
  case object If extends TokenData
  case object IntType extends TokenData
  case object New extends TokenData
  case object Null extends TokenData
  case object Public extends TokenData
  case object Return extends TokenData
  case object Static extends TokenData
  case object This extends TokenData
  case object True extends TokenData
  case object VoidType extends TokenData
  case object While extends TokenData

  // Non-alphanumeric Tokens
  case object Unequal extends TokenData
  case object Not extends TokenData
  case object ParenOpen extends TokenData
  case object ParenClosed extends TokenData
  case object Mult extends TokenData
  case object Plus extends TokenData
  case object Comma extends TokenData
  case object Minus extends TokenData
  case object Dot extends TokenData
  case object Divide extends TokenData
  case object Semicolon extends TokenData
  case object SmallerEquals extends TokenData
  case object Smaller extends TokenData
  case object Equals extends TokenData
  case object Assign extends TokenData
  case object GreaterEquals extends TokenData
  case object Greater extends TokenData
  case object Modulo extends TokenData
  case object LogicalAnd extends TokenData
  case object SquareBracketOpen extends TokenData
  case object SquareBracketClosed extends TokenData
  case object CurlyBraceOpen extends TokenData
  case object CurlyBraceClosed extends TokenData
  case object LogicalOr extends TokenData

  case class Identifier(value: String) extends TokenData
  case class IntegerLiteral(value: Int) extends TokenData

  // List of unused, but recognized miniJava keywords:
  // abstract, assert, break, byte, case, catch, char, const, continue, default, double, do, enum, extends, finally,
  // final, float, for, goto, implements, import, instanceof, interface, long, native, package, private, protected,
  // short, strictfp, super, switch, synchronized, throws, throw, transient, try, volatile
  // We have to recognize these and throw an error if someone uses these as identifier.
  //
  // List of unused, but recognized miniJava operators:
  // *= ++ += -= -- /= : <<= << >>= >>>= >>> >> ? %= &= & ^= ^ ~ |
  case class UnusedFeature(value: String) extends TokenData

}

class Token(val data: TokenData, val file: String, val line: Int, val char: Int)
