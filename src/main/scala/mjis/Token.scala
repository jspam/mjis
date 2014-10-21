package mjis

abstract class TokenData {}

object TokenData {

  // Keyword case classes
  case class BooleanType() extends TokenData
  case class Class() extends TokenData
  case class Else() extends TokenData
  case class False() extends TokenData
  case class If() extends TokenData
  case class Import() extends TokenData
  case class IntType() extends TokenData
  case class New() extends TokenData
  case class Null() extends TokenData
  case class Public() extends TokenData
  case class Return() extends TokenData
  case class Static() extends TokenData
  case class This() extends TokenData
  case class True() extends TokenData
  case class VoidType() extends TokenData
  case class While() extends TokenData

  // Non-alphanumeric TokenDatas
  case class Unequal() extends TokenData
  case class Not() extends TokenData
  case class ParenOpen() extends TokenData
  case class ParenClosed() extends TokenData
  case class MultAssign() extends TokenData
  case class Mult() extends TokenData
  case class Increment() extends TokenData
  case class PlusAssign() extends TokenData
  case class Plus() extends TokenData
  case class Comma() extends TokenData
  case class MinusAssign() extends TokenData
  case class Decrement() extends TokenData
  case class Minus() extends TokenData
  case class Dot() extends TokenData
  case class DivideAssign() extends TokenData
  case class Divide() extends TokenData
  case class Colon() extends TokenData
  case class Semicolon() extends TokenData
  case class ShiftLeftAssign() extends TokenData
  case class ShiftLeft() extends TokenData
  case class SmallerEquals() extends TokenData
  case class Smaller() extends TokenData
  case class Equals() extends TokenData
  case class Assign() extends TokenData
  case class GreaterEquals() extends TokenData
  case class ShiftRightAssign() extends TokenData
  case class LogicalShiftRightAssign() extends TokenData
  case class LogicalShiftRight() extends TokenData
  case class ShiftRight() extends TokenData
  case class Greater() extends TokenData
  case class QuestionMark() extends TokenData
  case class ModuloAssign() extends TokenData
  case class Modulo() extends TokenData
  case class AndAssign() extends TokenData
  case class LogicalAnd() extends TokenData
  case class BinaryAnd() extends TokenData
  case class SquareBracketOpen() extends TokenData
  case class SquareBracketClosed() extends TokenData
  case class XorAssign() extends TokenData
  case class Xor() extends TokenData
  case class CurlyBraceOpen() extends TokenData
  case class CurlyBraceClosed() extends TokenData
  case class BinaryNegate() extends TokenData
  case class OrAssign() extends TokenData
  case class LogicalOr() extends TokenData
  case class BinaryOr() extends TokenData
  case class Identifier(value: String) extends TokenData
  case class IntegerLiteral(value: Int) extends TokenData

  // List of unused, but recognized miniJava keywords:
  // abstract, assert, break, byte, case, catch, char, const, continue, default, double, do, enum, extends, finally,
  // final, float, for, goto, implements, instanceof, interface, long, native, package, private, protected, short,
  // strictfp, super, switch, synchronized, throws, throw, transient, try, volatile
  // We have to recognize these and throw an error if someone uses these as identifier
  case class UnusedKeyword(value: String) extends TokenData

}

class Token(val data : TokenData, val file : String, val line : Int, val char : Int) {
}
