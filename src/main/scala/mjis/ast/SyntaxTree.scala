package mjis.ast

// NOTE: Representing names and types with String is not a good idea,
//       will be fixed as soon as appropriate types are implemented.

/** Base trait for all syntax tree elements. */
sealed trait SyntaxTree

/** A syntax tree element that has a reference to a Decl. */
sealed trait Ref[D <: Decl] extends SyntaxTree {
  private var _decl: Option[D] = None
  def decl = _decl
  def decl_=(value: D) = _decl match {
    case None => _decl = Some(value)
    case Some(decl) => if (decl != value) throw new IllegalStateException("Tried to set two different declarations for one Ref.")
  }
}

final case class Program(classes: List[ClassDecl]) extends SyntaxTree

sealed trait Decl extends SyntaxTree {
  def name: String
}

/* class `name` {
   *   `methods`
   *   `fields`
   * }
   */
final case class ClassDecl(
  name: String,
  methods: List[MethodDecl],
  fields: List[FieldDecl]) extends Decl

sealed trait TypedDecl extends Decl {
  val typ: TypeDef
}

/* `typ` `name` (= `body`) */
final case class FieldDecl(
  name: String,
  override val typ: TypeDef) extends TypedDecl

/* `typ` `name`(`parameters`) { `body` } */
final case class MethodDecl(
  name: String,
  parameters: List[Parameter],
  override val typ: TypeDef,
  body: Block,
  isStatic: Boolean = false) extends TypedDecl

/* `typ` `name` */
final case class Parameter(name: String, override val typ: TypeDef) extends TypedDecl

sealed trait TypeDef extends SyntaxTree
final case class TypeBasic(name: String) extends TypeDef
final case class TypeArray(elementType: TypeBasic, numDimensions: Int = 1) extends TypeDef

sealed trait Statement extends SyntaxTree

/* `typ` `name` ( = `body`) */
final case class LocalVarDeclStatement(name: String, typ: TypeDef, initializer: Option[Expression]) extends Statement with Decl
final case class Block(statements: List[Statement]) extends Statement
case object EmptyStatement extends Statement
/* if (`condition`) { `ifTrue` } else { `ifFalse` } */
final case class If(condition: Expression, ifTrue: Statement, ifFalse: Statement) extends Statement
/* while (`condition`) { `block` } */
final case class While(condition: Expression, body: Statement) extends Statement
final case class ExpressionStatement(expr: Expression) extends Statement
final case class ReturnStatement(returnValue: Option[Expression]) extends Statement

sealed trait Expression extends SyntaxTree

/* `lhs` = `rhs` */
final case class Assignment(lhs: Expression, rhs: Expression) extends Expression
/* `name`(`args`) */
final case class Apply(name: String, arguments: List[Expression], isOperator: Boolean = false) extends Expression with Ref[MethodDecl]
/* new `typ`() */
final case class NewObject(typ: TypeBasic) extends Expression
/* new `typ`[firstDimSize][]^additionalDims */
final case class NewArray(typ: TypeBasic, firstDimSize: Expression, additionalDims: Int) extends Expression
/* `qualifier`.`name` */
final case class Select(qualifier: Expression, name: String) extends Expression with Ref[FieldDecl]

sealed trait Literal extends Expression

final case class Ident(name: String) extends Literal with Ref[Decl] {
  override def toString = name
}
final case class ThisLiteral() extends Literal with Ref[Decl]
case object NullLiteral extends Literal
final case class IntLiteral(value: String) extends Literal
sealed abstract class BooleanLiteral(value: Boolean) extends Literal
case object TrueLiteral extends BooleanLiteral(true)
case object FalseLiteral extends BooleanLiteral(false)

