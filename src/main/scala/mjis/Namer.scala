package mjis

import java.io.BufferedWriter

import mjis.Namer._
import mjis.ast._

import scala.collection.mutable

object Namer {

  case class DuplicateDefinitionError(firstDecl: Decl, secondDecl: Decl) extends SyntaxTreeError {
    def msg = s"${secondDecl.name} already defined" // TODO: "at ${firstDecl.pos}"
  }

  case class InvalidMainMethodNameError() extends SyntaxTreeError {
    def msg = s"'main' is the only name allowed for static methods"
  }

  case class NoMainMethodError() extends SyntaxTreeError {
    def msg = "No main method found"
  }

  case class DefNotFoundError(ident: String, defType: String) extends SyntaxTreeError {
    def msg = s"Not found: $defType $ident"
  }
}

class Namer(val input: Program) extends AnalysisPhase[Program] {

  private case class ResolveException(finding: Finding) extends Exception
  private class ClassLookup(val cls: ClassDecl, val fields: Map[String, FieldDecl], val methods: Map[String, MethodDecl])

  private class NamerVisitor extends PlainRecursiveVisitor[Unit, Unit, Unit]((), (), ()) {

    // mind the order: local classes may shadow builtin classes
    private val classes: Map[String, ClassLookup] = mkPackageLookup(Builtins.PublicTypeDecls) ++
      mkPackageLookup(input.classes)
    private val values = new SymbolTable()
    private val operators = mkDeclLookup(Builtins.Operators)

    private def mkPackageLookup(classes: List[ClassDecl]): Map[String, ClassLookup] = mkDeclLookup(classes) map {
      case (name, cls) => name -> new ClassLookup(cls, mkDeclLookup(cls.fields), mkDeclLookup(cls.methods))
    }

    private def mkDeclLookup[D <: Decl](xs: List[D]): Map[String, D] = xs groupBy (_.name) map {
      case (name, List(decl)) => name -> decl
      case (_, firstDecl :: secondDecl :: _) => throw ResolveException(DuplicateDefinitionError(firstDecl, secondDecl))
    }

    private def setDecl[A <: Decl](ref: Ref[A], value: Option[A], refType: String): Unit = value match {
      case None => throw ResolveException(DefNotFoundError(ref.name, refType))
      case Some(value) => ref.decl = value
    }

    private def resolveType(expr: Expression): TypeBasic = Typer.getType(expr) match {
      case ta: TypeArray => ta.elementType // let Typer worry about this
      case tb: TypeBasic => tb
    }

    override def postVisit(prog: Program): Unit =
      if (prog.mainMethodDecl.isEmpty) throw new ResolveException(NoMainMethodError())

    override def visit(typ: TypeBasic): Unit = setDecl(typ, classes.get(typ.name).map(_.cls), "type")

    override def preVisit(cls: ClassDecl): Unit = {
      values.enterScope()
      cls.fields.foreach(values.insert)
    }

    override def visit(method: MethodDecl): Unit = {
      values.enterScope()
      if (method.isStatic) {
        // Do not insert the parameter as it may not be accessed.
        if (method.name != "main") throw new ResolveException(InvalidMainMethodNameError())
        input.mainMethodDecl match {
          case Some(existingMain) =>
            if (!(existingMain eq method)) throw new ResolveException(DuplicateDefinitionError(existingMain, method))
          case None => input.mainMethodDecl = Some(method)
        }
        visit(method.body) // do not visit the arguments since 'String' is not defined in general
      } else {
        method.parameters.foreach(values.insert)
        super.visit(method)
      }
      values.leaveScope()
    }

    override def preVisit(stmt: Block): Unit = values.enterScope()
    override def postVisit(stmt: Block, _1: List[Unit]): Unit = values.leaveScope()

    override def preVisit(stmt: LocalVarDeclStatement): Unit = {
      // LocalVarDecls may shadow field declarations, but not other LocalVarDecls or parameters
      values.lookup(stmt.name) match {
        case None | Some(FieldDecl(_, _)) =>
        case _ => throw new ResolveException(DuplicateDefinitionError(values.lookup(stmt.name).get, stmt))
      }

      values.insert(stmt)
      // Visit the initializer *after* the value has been inserted -- it must be visible there.
      //   JLS 14.4.2: "The scope of a local variable declaration in a block (§14.2)
      //   is the rest of the block in which the declaration appears,
      //   starting with its own initializer (§14.4) [...]"
      // 'int x = x;' is only invalid Java because of definite assignment rules, which do not
      // apply to MiniJava.
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
            expr.arguments(0).asInstanceOf[Select].qualifier.asInstanceOf[Ident].decl = Builtins.SystemDecl
            expr.arguments(0).asInstanceOf[Select].decl = Builtins.SystemOutFieldDecl
            // do _not_ visit arguments(0)
            expr.arguments.tail.foreach(_.accept(this))
          } else {
            super.visit(expr)
            val qualifierType = resolveType(expr.arguments(0))
            setDecl(expr, classes(qualifierType.name).methods.get(name), "method")
          }
      }
    }

    override def postVisit(expr: Select, _1: Unit): Unit = {
      val thisType = resolveType(expr.qualifier)
      setDecl(expr, classes(thisType.name).fields.get(expr.name), "field")
    }

    override def visit(expr: Ident): Unit = setDecl(expr, values.lookup(expr.name), "value")

    override def visit(expr: ThisLiteral): Unit = {
      // lookup can only be None (for static methods) or the this parameter
      setDecl(expr, values.lookup("this").map(_.asInstanceOf[Parameter]), "value")
    }
  }

  override protected def getResult(): Program = { resolve(); input }
  override def dumpResult(writer: BufferedWriter): Unit = {} // no stdout output, just the error code

  private val _findings = mutable.ListBuffer.empty[Finding]
  override def findings: List[Finding] = _findings.toList

  private def resolve(): Unit = {
    try {
      input.accept(new NamerVisitor())
    } catch {
      case ResolveException(finding) => _findings += finding
    }
  }
}
