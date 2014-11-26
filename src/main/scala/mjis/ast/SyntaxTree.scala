package mjis.ast

import mjis._

/** Base trait for all syntax tree elements. */
sealed trait SyntaxTree {
  def pos: Position
}

abstract class SyntaxTreeError( /* element: SyntaxTree */ ) extends Finding {
  def severity = Severity.ERROR
  override def pos: Position = Position(0, 0, "") // TODO: element.position
}

/** A syntax tree element that has a reference to a Decl. */
sealed trait Ref[D <: Decl] extends SyntaxTree {
  private var _decl: Option[D] = None
  def decl = _decl
  def decl_=(value: D) = _decl match {
    case None       => _decl = Some(value)
    case Some(decl) => if (decl != value) throw new IllegalStateException("Tried to set two different declarations for one Ref.")
  }
  val name: String
}

final case class Program(classes: List[ClassDecl])(implicit val pos: Position) extends SyntaxTree {
  var mainMethodDecl: Option[MethodDecl] = None
  def accept(visitor: ProgramVisitor): Unit = visitor.visit(this)
}

sealed trait Decl extends SyntaxTree {
  def name: String
  def isReadable = true
  def isWritable = false
}

/** class `name` {
  *   `methods`
  *   `fields`
  * }
  */
final case class ClassDecl(
  name: String,
  methods: List[MethodDecl],
  fields: List[FieldDecl])(implicit val pos: Position) extends Decl {

  def accept(visitor: ProgramVisitor): Unit = visitor.visit(this)
}

sealed trait TypedDecl extends Decl {
  val typ: TypeDef
}

sealed trait MemberDecl extends TypedDecl

/* `typ` `name` (= `body`) */
final case class FieldDecl(
  name: String,
  override val typ: TypeDef)(implicit val pos: Position) extends MemberDecl {

  def accept(visitor: ProgramVisitor): Unit = visitor.visit(this)
  override def isWritable = true
}

/* `typ` `name`(`parameters`) { `body` } */
final case class MethodDecl(
  name: String,
  var parameters: List[Parameter],
  override val typ: TypeDef,
  body: Block,
  isStatic: Boolean = false)(implicit val pos: Position) extends MemberDecl {

  var numVars = parameters.length
  def accept(visitor: ProgramVisitor): Unit = visitor.visit(this)
  override def isReadable = !isStatic /* for methods, isReadable == isCallable */
}

/* `typ` `name` */
final case class Parameter(
    name: String, override val typ: TypeDef,
    override val isReadable: Boolean = true,
    override val isWritable: Boolean = true)(implicit val pos: Position) extends TypedDecl {

  def accept(visitor: ProgramVisitor): Unit = visitor.visit(this)
}

sealed trait TypeDef extends SyntaxTree {
  def accept[T](visitor: TypeVisitor[T]): T
}

final case class TypeBasic(name: String)(implicit val pos: Position) extends TypeDef with Ref[ClassDecl] {
  override def accept[T](visitor: TypeVisitor[T]): T = visitor.visit(this)
}
final case class TypeArray(elementType: TypeBasic, numDimensions: Int = 1)(implicit val pos: Position) extends TypeDef {
  override def accept[T](visitor: TypeVisitor[T]): T = visitor.visit(this)
}

sealed trait Statement extends SyntaxTree {
  def accept[S](visitor: StatementVisitor[S]): S
  def isEmpty = false
}

/* `typ` `name` ( = `body`) */
final case class LocalVarDeclStatement(
    name: String,
    override val typ: TypeDef,
    initializer: Option[Expression])(implicit val pos: Position) extends Statement with TypedDecl {
  override def accept[S](visitor: StatementVisitor[S]): S = visitor.visit(this)
  override def isWritable = true
}
final case class Block(statements: List[Statement])(implicit val pos: Position) extends Statement {
  override def accept[S](visitor: StatementVisitor[S]): S = visitor.visit(this)
}
case class EmptyStatement()(implicit val pos: Position) extends Statement {
  override def accept[S](visitor: StatementVisitor[S]): S = visitor.visit(this)
  override def isEmpty = true
}
/* if (`condition`) { `ifTrue` } else { `ifFalse` } */
final case class If(condition: Expression, ifTrue: Statement, ifFalse: Statement)(implicit val pos: Position) extends Statement {
  override def accept[S](visitor: StatementVisitor[S]): S = visitor.visit(this)
}
/* while (`condition`) { `block` } */
final case class While(condition: Expression, body: Statement)(implicit val pos: Position) extends Statement {
  override def accept[S](visitor: StatementVisitor[S]): S = visitor.visit(this)
}
final case class ExpressionStatement(expr: Expression)(implicit val pos: Position) extends Statement {
  override def accept[S](visitor: StatementVisitor[S]): S = visitor.visit(this)
}
final case class ReturnStatement(returnValue: Option[Expression])(implicit val pos: Position) extends Statement {
  override def accept[S](visitor: StatementVisitor[S]): S = visitor.visit(this)
}

sealed trait Expression extends SyntaxTree {
  def accept[E](visitor: ExpressionVisitor[E]): E
}

/* `lhs` = `rhs` */
final case class Assignment(lhs: Expression, rhs: Expression)(implicit val pos: Position) extends Expression {
  override def accept[E](visitor: ExpressionVisitor[E]): E = visitor.visit(this)
}
/* `name`(`args`) */
final case class Apply(name: String, var arguments: List[Expression], isOperator: Boolean = false)(implicit val pos: Position) extends Expression with Ref[MethodDecl] {
  override def accept[E](visitor: ExpressionVisitor[E]): E = visitor.visit(this)
}
/* new `typ`() */
final case class NewObject(typ: TypeBasic)(implicit val pos: Position) extends Expression {
  override def accept[E](visitor: ExpressionVisitor[E]): E = visitor.visit(this)
}
/* new `typ`[firstDimSize][]^additionalDims */
final case class NewArray(typ: TypeBasic, firstDimSize: Expression, additionalDims: Int)(implicit val pos: Position) extends Expression {
  override def accept[E](visitor: ExpressionVisitor[E]): E = visitor.visit(this)
}
/* `qualifier`.`name` */
final case class Select(qualifier: Expression, name: String)(implicit val pos: Position) extends Expression with Ref[FieldDecl] {
  override def accept[E](visitor: ExpressionVisitor[E]): E = visitor.visit(this)
}

sealed trait Literal extends Expression

final case class Ident(name: String)(implicit val pos: Position) extends Literal with Ref[Decl] {
  override def accept[E](visitor: ExpressionVisitor[E]): E = visitor.visit(this)
  override def toString = name
}
final case class ThisLiteral()(implicit val pos: Position) extends Literal with Ref[TypedDecl] {
  override def accept[E](visitor: ExpressionVisitor[E]): E = visitor.visit(this)
  override val name = "this"
}
case class NullLiteral()(implicit val pos: Position) extends Literal {
  override def accept[E](visitor: ExpressionVisitor[E]): E = visitor.visit(this)
}
final case class IntLiteral(value: String)(implicit val pos: Position) extends Literal {
  override def accept[E](visitor: ExpressionVisitor[E]): E = visitor.visit(this)
}
sealed abstract class BooleanLiteral(value: Boolean)(implicit val pos: Position) extends Literal {
  override def accept[E](visitor: ExpressionVisitor[E]): E = visitor.visit(this)
}
case class TrueLiteral()(override implicit val pos: Position) extends BooleanLiteral(true)(pos)
case class FalseLiteral()(override implicit val pos: Position) extends BooleanLiteral(false)(pos)

