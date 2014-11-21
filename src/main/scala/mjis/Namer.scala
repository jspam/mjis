package mjis

import java.io.BufferedWriter

import mjis.Namer.{DefNotFoundError, DuplicateDefinitionError}
import mjis.ast._

import scala.collection.mutable

object Namer {

  case class DuplicateDefinitionError(firstDecl: Decl, secondDecl: Decl) extends SyntaxTreeError {
    def msg = s"${secondDecl.name} already defined" // TODO: "at ${firstDecl.pos}"
  }

  case class DefNotFoundError(ident: String, defType: String) extends SyntaxTreeError {
    def msg = s"Not found: $defType $ident"
  }
}

class Namer(val input: Program) extends AnalysisPhase[Program] {

  private case class ResolveException(finding: Finding) extends Exception
  private class ClassLookup(val cls: ClassDecl, val fields: Map[String, FieldDecl], val methods: Map[String, MethodDecl])

  private class NamerVisitor extends PostOrderVisitor(new NullVisitor()) {

    private def setDecl[A <: Decl](ref: Ref[A], value: Option[A], refType: String): Unit = value match {
      case None => throw ResolveException(DefNotFoundError(ref.name, refType))
      case Some(value) => ref.decl = value
    }

    private def resolveType(expr: Expression): TypeBasic = Typer.getType(expr) match {
      case ta: TypeArray => ta.elementType // let Typer worry about this
      case tb: TypeBasic => tb
    }

    override def visit(typ: TypeBasic): Unit = setDecl(typ, classes.get(typ.name).map(_.cls), "type")
    override def visit(cls: ClassDecl): Unit = {
      values.enterScope()
      cls.fields.foreach(values.insert)
      super.visit(cls)
      values.leaveScope()
    }
    override def visit(method: MethodDecl): Unit = {
      values.enterScope()
      method.parameters.foreach(values.insert)
      super.visit(method)
      values.leaveScope()
    }
    override def visit(stmt: Block): Unit = {
      values.enterScope()
      super.visit(stmt)
      values.leaveScope()
    }
    override def visit(stmt: LocalVarDeclStatement): Unit = {
      super.visit(stmt)
      if (values.inCurrentScope(stmt.name))
        throw new ResolveException(DuplicateDefinitionError(values.lookup(stmt.name).get, stmt))
      values.insert(stmt)
    }
    override def visit(expr: Apply): Unit = {
      val name = if (expr.name == "-" && expr.arguments.length == 1) "- (unary)" else expr.name
      operators.get(name) match {
        case Some(op) =>
          super.visit(expr)
          expr.decl = op
        case None =>
          if (expr.name == "println" &&
            expr.arguments(0) == Select(Ident("System"), "out") &&
            values.lookup("System") == None) {
            expr.decl = Builtins.SystemOutPrintlnDecl
            // do _not_ visit arguments(0)
            expr.arguments.tail.foreach(_.accept(this))
          } else {
            super.visit(expr)
            val qualifierType = resolveType(expr.arguments(0))
            setDecl(expr, classes(qualifierType.name).methods.get(name), "method")
          }
      }
    }
    override def visit(expr: Select): Unit = {
      super.visit(expr)
      val thisType = resolveType(expr.qualifier)
      setDecl(expr, classes(thisType.name).fields.get(expr.name), "field")
    }
    override def visit(expr: Ident): Unit = {
      super.visit(expr)
      setDecl(expr, values.lookup(expr.name), "value")
    }
    override def visit(expr: ThisLiteral): Unit = {
      super.visit(expr)
      // lookup can only be None (for static methods) or the this parameter
      setDecl(expr, values.lookup("this").map(_.asInstanceOf[Parameter]), "value")
    }
  }

  override protected def getResult(): Program = { resolve(); input }
  override def dumpResult(writer: BufferedWriter): Unit = ???

  private val _findings = mutable.ListBuffer.empty[Finding]
  override def findings: List[Finding] = _findings.toList

  // mind the order: local classes may shadow builtin classes
  private lazy val classes: Map[String, ClassLookup] = mkPackageLookup(Builtins.PublicTypeDecls) ++
    mkPackageLookup(input.classes)
  private val values = new SymbolTable()
  private val operators = mkDeclLookup(Builtins.Operators)

  private def mkPackageLookup(classes: List[ClassDecl]): Map[String, ClassLookup] = mkDeclLookup(classes) mapValues (cls =>
    new ClassLookup(cls, mkDeclLookup(cls.fields), mkDeclLookup(cls.methods))
  )

  private def mkDeclLookup[D <: Decl](xs: List[D]): Map[String, D] = xs groupBy (_.name) mapValues {
    case List(decl) => decl
    case firstDecl :: secondDecl :: _ => throw ResolveException(DuplicateDefinitionError(firstDecl, secondDecl))
  }

  private def resolve(): Unit = {
    try {
      input.accept(new NamerVisitor())
    } catch {
      case ResolveException(finding) => _findings += finding
    }
  }
}
