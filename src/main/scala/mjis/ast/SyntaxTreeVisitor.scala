package mjis.ast

import scala.util.control.TailCalls._
import mjis.util.ListOfTailRecExtensions._

trait ProgramVisitor {
  def visit(program: Program): Unit
  def visit(cls: ClassDecl): Unit
  def visit(method: MethodDecl): Unit
  def visit(field: FieldDecl): Unit
  def visit(param: Parameter): Unit
}

trait TypeVisitor[T] {
  def visit(typ: TypeBasic): T
  def visit(typ: TypeArray): T
}

trait StatementVisitor[S] {
  def visit(stmt: Block): S
  def visit(stmt: EmptyStatement): S
  def visit(stmt: If): S
  def visit(stmt: While): S
  def visit(stmt: LocalVarDeclStatement): S
  def visit(stmt: ReturnStatement): S
  def visit(stmt: ExpressionStatement): S
}

trait ExpressionVisitor[E] {
  def visit(expr: Apply): E
  def visit(expr: Assignment): E
  def visit(expr: BooleanLiteral): E
  def visit(expr: Ident): E
  def visit(expr: IntLiteral): E
  def visit(expr: NullLiteral): E
  def visit(expr: NewArray): E
  def visit(expr: NewObject): E
  def visit(expr: Select): E
  def visit(expr: ThisLiteral): E
}

class ProgramVisitorPrePost[T, S, E](defaultT: T, defaultS: S, defaultE: E) {

  def preVisit(program: Program): Unit = {}
  def postVisit(program: Program): Unit = {}

  def preVisit(cls: ClassDecl): Unit = {}
  def postVisit(cls: ClassDecl): Unit = {}

  def preVisit(method: MethodDecl): Unit = {}
  def postVisit(method: MethodDecl, typResult: T, bodyResult: S): Unit = {}

  def preVisit(field: FieldDecl): Unit = {}
  def postVisit(field: FieldDecl, typResult: T): Unit = {}

  def preVisit(param: Parameter): Unit = {}
  def postVisit(param: Parameter, typResult: T): Unit = {}

  def postVisit(typ: TypeBasic): T = defaultT

  def preVisit(typ: TypeArray): Unit = {}
  def postVisit(typ: TypeArray, elementTypeResult: T): T = defaultT

  def preVisit(stmt: Block): Unit = {}
  def postVisit(stmt: Block, statementResults: List[S]): S = defaultS

  def postVisit(stmt: EmptyStatement): S = defaultS

  def preVisit(stmt: If): Unit = {}
  def postVisit(stmt: If, condResult: E, ifTrueResult: S, ifFalseResult: S): S = defaultS

  def preVisit(stmt: While): Unit = {}
  def postVisit(stmt: While, condResult: E, bodyResult: S): S = defaultS

  def preVisit(stmt: LocalVarDeclStatement): Unit = {}
  def postVisit(stmt: LocalVarDeclStatement, typResult: T, initializerResult: Option[E]): S = defaultS

  def preVisit(stmt: ReturnStatement): Unit = {}
  def postVisit(stmt: ReturnStatement, exprResult: Option[E]): S = defaultS

  def preVisit(stmt: ExpressionStatement): Unit = {}
  def postVisit(stmt: ExpressionStatement, exprResult: E): S = defaultS

  def preVisit(expr: Apply): Unit = {}
  def postVisit(expr: Apply, argumentResults: List[E]): E = defaultE

  def preVisit(expr: Assignment): Unit = {}
  def postVisit(expr: Assignment, lhsResult: E, rhsResult: E): E = defaultE

  def postVisit(expr: BooleanLiteral): E = defaultE

  def postVisit(expr: Ident): E = defaultE

  def postVisit(expr: IntLiteral): E = defaultE

  def postVisit(stmt: NullLiteral): E = defaultE

  def postVisit(expr: ThisLiteral): E = defaultE

  def preVisit(expr: NewArray): Unit = {}
  def postVisit(expr: NewArray, typResult: T, firstDimSizeResult: E): E = defaultE

  def preVisit(expr: NewObject): Unit = {}
  def postVisit(expr: NewObject, typResult: T): E = defaultE

  def preVisit(expr: Select): Unit = {}
  def postVisit(expr: Select, qualifierResult: E): E = defaultE
}

class PlainRecursiveVisitor[T, S, E](defaultT: T, defaultS: S, defaultE: E)
  extends ProgramVisitorPrePost[T, S, E](defaultT, defaultS, defaultE)
  with ProgramVisitor
  with TypeVisitor[T]
  with StatementVisitor[S]
  with ExpressionVisitor[E] {

  def visit(program: Program): Unit = {
    preVisit(program)
    program.classes.foreach(visit)
    postVisit(program)
  }

  def visit(cls: ClassDecl): Unit = {
    preVisit(cls)
    cls.fields.foreach(visit)
    cls.methods.foreach(visit)
    postVisit(cls)
  }

  def visit(method: MethodDecl): Unit = {
    preVisit(method)
    method.parameters.map(visit)
    postVisit(method, method.returnType.accept(this), visit(method.body))
  }

  def visit(field: FieldDecl): Unit = {
    preVisit(field)
    postVisit(field, field.typ.accept(this))
  }

  def visit(param: Parameter): Unit = {
    preVisit(param)
    postVisit(param, param.typ.accept(this))
  }

  def visit(typ: TypeBasic): T = postVisit(typ)

  def visit(typ: TypeArray): T = {
    preVisit(typ)
    postVisit(typ, visit(typ.elementType))
  }

  def visit(stmt: Block): S = {
    preVisit(stmt)
    postVisit(stmt, stmt.statements.map(_.accept(this)))
  }

  def visit(stmt: EmptyStatement): S = postVisit(stmt)

  def visit(stmt: If): S = {
    preVisit(stmt)
    postVisit(stmt, stmt.condition.accept(this), stmt.ifFalse.accept(this), stmt.ifTrue.accept(this))
  }

  def visit(stmt: While): S = {
    preVisit(stmt)
    postVisit(stmt, stmt.condition.accept(this), stmt.body.accept(this))
  }

  def visit(stmt: LocalVarDeclStatement): S = {
    preVisit(stmt)
    postVisit(stmt, stmt.typ.accept(this), stmt.initializer.map(_.accept(this)))
  }

  def visit(stmt: ReturnStatement): S = {
    preVisit(stmt)
    postVisit(stmt, stmt.returnValue.map(_.accept(this)))
  }

  def visit(stmt: ExpressionStatement): S = {
    preVisit(stmt)
    postVisit(stmt, stmt.expr.accept(this))
  }

  def visit(expr: Apply): E = {
    preVisit(expr)
    postVisit(expr, expr.arguments.map(_.accept(this)))
  }

  def visit(expr: Assignment): E = {
    preVisit(expr)
    postVisit(expr, expr.lhs.accept(this), expr.rhs.accept(this))
  }

  def visit(expr: BooleanLiteral): E = postVisit(expr)

  def visit(expr: Ident): E = postVisit(expr)

  def visit(expr: IntLiteral): E = postVisit(expr)

  def visit(expr: NullLiteral): E = postVisit(expr)

  def visit(expr: ThisLiteral): E = postVisit(expr)

  def visit(expr: NewArray): E = {
    preVisit(expr)
    postVisit(expr, expr.baseType.accept(this), expr.firstDimSize.accept(this))
  }

  def visit(expr: NewObject): E = {
    preVisit(expr)
    postVisit(expr, expr.typ.accept(this))
  }

  def visit(expr: Select): E = {
    preVisit(expr)
    postVisit(expr, expr.qualifier.accept(this))
  }
}

class TailRecursiveVisitor[T, S, E](defaultT: T, defaultS: S, defaultE: E)
  extends ProgramVisitorPrePost[T, S, E](defaultT, defaultS, defaultE)
  with ProgramVisitor
  with TypeVisitor[T]
  with StatementVisitor[TailRec[S]]
  with ExpressionVisitor[TailRec[E]] {

  def visit(program: Program): Unit = {
    preVisit(program)
    program.classes.foreach(visit)
    postVisit(program)
  }

  def visit(cls: ClassDecl): Unit = {
    preVisit(cls)
    cls.fields.foreach(visit)
    cls.methods.foreach(visit)
    postVisit(cls)
  }

  def visit(method: MethodDecl): Unit = {
    preVisit(method)
    method.parameters.foreach(visit)
    postVisit(method, method.returnType.accept(this), visit(method.body).result)
  }

  def visit(field: FieldDecl): Unit = {
    preVisit(field)
    postVisit(field, field.typ.accept(this))
  }

  def visit(param: Parameter): Unit = {
    preVisit(param)
    postVisit(param, param.typ.accept(this))
  }

  def visit(typ: TypeBasic): T = postVisit(typ)

  def visit(typ: TypeArray): T = {
    preVisit(typ)
    postVisit(typ, visit(typ.elementType))
  }

  def visit(stmt: Block): TailRec[S] = {
    preVisit(stmt)
    stmt.statements.sequenceMap(_.accept(this)).map(postVisit(stmt, _))
  }

  def visit(stmt: EmptyStatement): TailRec[S] = done(postVisit(stmt))

  def visit(stmt: If): TailRec[S] = {
    preVisit(stmt)
    for (condResult <- stmt.condition.accept(this);
         ifTrueResult <- stmt.ifFalse.accept(this);
         ifFalseResult <- stmt.ifTrue.accept(this))
      yield postVisit(stmt, condResult, ifTrueResult, ifFalseResult)
  }

  def visit(stmt: While): TailRec[S] = {
    preVisit(stmt)
    for (condResult <- stmt.condition.accept(this);
         bodyResult <- stmt.body.accept(this))
      yield postVisit(stmt, condResult, bodyResult)
  }

  def visit(stmt: LocalVarDeclStatement): TailRec[S] = {
    preVisit(stmt)
    done(postVisit(stmt, stmt.typ.accept(this), stmt.initializer.map(_.accept(this).result)))
  }

  def visit(stmt: ReturnStatement): TailRec[S] = {
    preVisit(stmt)
    done(postVisit(stmt, stmt.returnValue.map(_.accept(this).result)))
  }

  def visit(stmt: ExpressionStatement): TailRec[S] = {
    preVisit(stmt)
    done(postVisit(stmt, stmt.expr.accept(this).result))
  }

  def visit(expr: Apply): TailRec[E] = {
    preVisit(expr)
    expr.arguments.sequenceMap(_.accept(this)).map(postVisit(expr, _))
  }

  def visit(expr: Assignment): TailRec[E] = {
    preVisit(expr)
    for (lhsResult <- expr.lhs.accept(this);
         rhsResult <- expr.rhs.accept(this))
      yield postVisit(expr, lhsResult, rhsResult)
  }

  def visit(expr: BooleanLiteral): TailRec[E] = done(postVisit(expr))

  def visit(expr: Ident): TailRec[E] = done(postVisit(expr))

  def visit(expr: IntLiteral): TailRec[E] = done(postVisit(expr))

  def visit(expr: NullLiteral): TailRec[E] = done(postVisit(expr))

  def visit(expr: ThisLiteral): TailRec[E] = done(postVisit(expr))

  def visit(expr: NewArray): TailRec[E] = {
    preVisit(expr)
    expr.firstDimSize.accept(this).map(postVisit(expr, expr.baseType.accept(this), _))
  }

  def visit(expr: NewObject): TailRec[E] = {
    preVisit(expr)
    done(postVisit(expr, expr.typ.accept(this)))
  }

  def visit(expr: Select): TailRec[E] = {
    preVisit(expr)
    expr.qualifier.accept(this).map(postVisit(expr, _))
  }
}

