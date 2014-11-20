package mjis.ast

trait SyntaxTreeVisitor[A] {
  def visit(program: Program): A
  def visit(cls: ClassDecl): A
  def visit(method: MethodDecl): A
  def visit(field: FieldDecl): A
  def visit(param: Parameter): A
  def visit(typ: TypeBasic): A
  def visit(typ: TypeArray): A
  def visit(stmt: Block): A
  def visit(stmt: EmptyStatement.type): A
  def visit(stmt: If): A
  def visit(stmt: While): A
  def visit(stmt: LocalVarDeclStatement): A
  def visit(stmt: ReturnStatement): A
  def visit(stmt: ExpressionStatement): A
  def visit(expr: Apply): A
  def visit(expr: Assignment): A
  def visit(expr: BooleanLiteral): A
  def visit(expr: Ident): A
  def visit(expr: IntLiteral): A
  def visit(expr: NullLiteral.type): A
  def visit(expr: NewArray): A
  def visit(expr: NewObject): A
  def visit(expr: Select): A
  def visit(expr: ThisLiteral): A
}

class NullVisitor extends SyntaxTreeVisitor[Unit] {
  def visit(program: Program): Unit = {}
  def visit(cls: ClassDecl): Unit = {}
  def visit(method: MethodDecl): Unit = {}
  def visit(field: FieldDecl): Unit = {}
  def visit(param: Parameter): Unit = {}
  def visit(typ: TypeBasic): Unit = {}
  def visit(typ: TypeArray): Unit = {}
  def visit(stmt: Block): Unit = {}
  def visit(stmt: EmptyStatement.type): Unit = {}
  def visit(stmt: If): Unit = {}
  def visit(stmt: While): Unit = {}
  def visit(stmt: LocalVarDeclStatement): Unit = {}
  def visit(stmt: ReturnStatement): Unit = {}
  def visit(stmt: ExpressionStatement): Unit = {}
  def visit(expr: Apply): Unit = {}
  def visit(expr: Assignment): Unit = {}
  def visit(expr: BooleanLiteral): Unit = {}
  def visit(expr: Ident): Unit = {}
  def visit(expr: IntLiteral): Unit = {}
  def visit(stmt: NullLiteral.type): Unit = {}
  def visit(expr: NewArray): Unit = {}
  def visit(expr: NewObject): Unit = {}
  def visit(expr: Select): Unit = {}
  def visit(expr: ThisLiteral): Unit = {}
  def visitNullLiteral(): Unit = {}
  def visitThisLiteral(): Unit = {}
}

class PostOrderVisitor(inner: SyntaxTreeVisitor[Unit]) extends SyntaxTreeVisitor[Unit] {
  def visit(program: Program): Unit = {
    program.classes.foreach(visit)
    inner.visit(program)
  }
  def visit(cls: ClassDecl): Unit = {
    cls.fields.foreach(visit)
    cls.methods.foreach(visit)
    inner.visit(cls)
  }
  def visit(method: MethodDecl): Unit = {
    visit(method.body)
    inner.visit(method)
  }
  def visit(field: FieldDecl): Unit = inner.visit(field)
  def visit(param: Parameter): Unit = {
    param.typ.accept(this)
    inner.visit(param)
  }
  def visit(typ: TypeBasic): Unit = inner.visit(typ)
  def visit(typ: TypeArray): Unit = {
    visit(typ.elementType)
    inner.visit(typ)
  }
  def visit(stmt: Block): Unit = {
    stmt.statements.foreach(_.accept(this))
    inner.visit(stmt)
  }
  def visit(stmt: EmptyStatement.type): Unit = inner.visit(stmt)
  def visit(stmt: If): Unit = {
    stmt.condition.accept(this)
    stmt.ifFalse.accept(this)
    stmt.ifTrue.accept(this)
    inner.visit(stmt)
  }
  def visit(stmt: While): Unit = {
    stmt.condition.accept(this)
    stmt.body.accept(this)
    inner.visit(stmt)
  }
  def visit(stmt: LocalVarDeclStatement): Unit = {
    stmt.initializer.foreach(_.accept(this))
    inner.visit(stmt)
  }
  def visit(stmt: ReturnStatement): Unit = {
    stmt.returnValue.foreach(_.accept(this))
    inner.visit(stmt)
  }
  def visit(stmt: ExpressionStatement): Unit = {
    stmt.expr.accept(this)
    inner.visit(stmt)
  }
  def visit(expr: Apply): Unit = {
    expr.arguments.foreach(_.accept(this))
    inner.visit(expr)
  }
  def visit(expr: Assignment): Unit = {
    expr.lhs.accept(this)
    expr.rhs.accept(this)
    inner.visit(expr)
  }
  def visit(expr: BooleanLiteral): Unit = inner.visit(expr)
  def visit(expr: Ident): Unit = inner.visit(expr)
  def visit(expr: IntLiteral): Unit = inner.visit(expr)
  def visit(stmt: NullLiteral.type): Unit = inner.visit(stmt)
  def visit(expr: NewArray): Unit = {
    expr.firstDimSize.accept(this)
    inner.visit(expr)
  }
  def visit(expr: NewObject): Unit = inner.visit(expr)
  def visit(expr: Select): Unit = {
    expr.qualifier.accept(this)
    inner.visit(expr)
  }
  def visit(expr: ThisLiteral): Unit = inner.visit(expr)
}

// class TailRecPostOrderVisitor(inner: SyntaxTreeVisitor[Unit]) extends SyntaxTreeVisitor[TailRec[Unit]] {
// ...
