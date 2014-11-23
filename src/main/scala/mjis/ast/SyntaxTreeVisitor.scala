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

class RecursiveVisitor() extends SyntaxTreeVisitor[Unit] {

  def preVisit(program: Program): Unit = {}
  def postVisit(program: Program): Unit = {}
  def visit(program: Program): Unit = {
    preVisit(program)
    program.classes.foreach(visit)
    postVisit(program)
  }

  def preVisit(cls: ClassDecl): Unit = {}
  def postVisit(cls: ClassDecl): Unit = {}
  def visit(cls: ClassDecl): Unit = {
    preVisit(cls)
    cls.fields.foreach(visit)
    cls.methods.foreach(visit)
    postVisit(cls)
  }

  def preVisit(method: MethodDecl): Unit = {}
  def postVisit(method: MethodDecl): Unit = {}
  def visit(method: MethodDecl): Unit = {
    preVisit(method)
    method.parameters.foreach(visit)
    visit(method.body)
    postVisit(method)
  }

  def preVisit(field: FieldDecl): Unit = {}
  def postVisit(field: FieldDecl): Unit = {}
  def visit(field: FieldDecl): Unit = {
    preVisit(field)
    field.typ.accept(this)
    postVisit(field)
  }

  def preVisit(param: Parameter): Unit = {}
  def postVisit(param: Parameter): Unit = {}
  def visit(param: Parameter): Unit = {
    preVisit(param)
    param.typ.accept(this)
    postVisit(param)
  }

  def visit(typ: TypeBasic): Unit = {}

  def preVisit(typ: TypeArray): Unit = {}
  def postVisit(typ: TypeArray): Unit = {}
  def visit(typ: TypeArray): Unit = {
    preVisit(typ)
    visit(typ.elementType)
    postVisit(typ)
  }

  def preVisit(stmt: Block): Unit = {}
  def postVisit(stmt: Block): Unit = {}
  def visit(stmt: Block): Unit = {
    preVisit(stmt)
    stmt.statements.foreach(_.accept(this))
    postVisit(stmt)
  }

  def visit(stmt: EmptyStatement.type): Unit = {}

  def preVisit(stmt: If): Unit = {}
  def postVisit(stmt: If): Unit = {}
  def visit(stmt: If): Unit = {
    preVisit(stmt)
    stmt.condition.accept(this)
    stmt.ifFalse.accept(this)
    stmt.ifTrue.accept(this)
    postVisit(stmt)
  }

  def preVisit(stmt: While): Unit = {}
  def postVisit(stmt: While): Unit = {}
  def visit(stmt: While): Unit = {
    preVisit(stmt)
    stmt.condition.accept(this)
    stmt.body.accept(this)
    postVisit(stmt)
  }

  def preVisit(stmt: LocalVarDeclStatement): Unit = {}
  def postVisit(stmt: LocalVarDeclStatement): Unit = {}
  def visit(stmt: LocalVarDeclStatement): Unit = {
    preVisit(stmt)
    stmt.initializer.foreach(_.accept(this))
    postVisit(stmt)
  }

  def preVisit(stmt: ReturnStatement): Unit = {}
  def postVisit(stmt: ReturnStatement): Unit = {}
  def visit(stmt: ReturnStatement): Unit = {
    preVisit(stmt)
    stmt.returnValue.foreach(_.accept(this))
    postVisit(stmt)
  }

  def preVisit(stmt: ExpressionStatement): Unit = {}
  def postVisit(stmt: ExpressionStatement): Unit = {}
  def visit(stmt: ExpressionStatement): Unit = {
    preVisit(stmt)
    stmt.expr.accept(this)
    postVisit(stmt)
  }

  def preVisit(expr: Apply): Unit = {}
  def postVisit(expr: Apply): Unit = {}
  def visit(expr: Apply): Unit = {
    preVisit(expr)
    expr.arguments.foreach(_.accept(this))
    postVisit(expr)
  }

  def preVisit(expr: Assignment): Unit = {}
  def postVisit(expr: Assignment): Unit = {}
  def visit(expr: Assignment): Unit = {
    preVisit(expr)
    expr.lhs.accept(this)
    expr.rhs.accept(this)
    postVisit(expr)
  }

  def visit(expr: BooleanLiteral): Unit = {}

  def visit(expr: Ident): Unit = {}

  def visit(expr: IntLiteral): Unit = {}

  def visit(stmt: NullLiteral.type): Unit = {}

  def preVisit(expr: NewArray): Unit = {}
  def postVisit(expr: NewArray): Unit = {}
  def visit(expr: NewArray): Unit = {
    preVisit(expr)
    expr.typ.accept(this)
    expr.firstDimSize.accept(this)
    postVisit(expr)
  }

  def preVisit(expr: NewObject): Unit = {}
  def postVisit(expr: NewObject): Unit = {}
  def visit(expr: NewObject): Unit = {
    preVisit(expr)
    expr.typ.accept(this)
    postVisit(expr)
  }

  def preVisit(expr: Select): Unit = {}
  def postVisit(expr: Select): Unit = {}
  def visit(expr: Select): Unit = {
    preVisit(expr)
    expr.qualifier.accept(this)
    postVisit(expr)
  }

  def visit(expr: ThisLiteral): Unit = {}
}

// class TailRecPostOrderVisitor(inner: SyntaxTreeVisitor[Unit]) extends SyntaxTreeVisitor[TailRec[Unit]] {
// ...
