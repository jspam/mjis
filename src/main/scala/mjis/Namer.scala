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

  case class InaccessibleDeclError(decl: Decl) extends SyntaxTreeError {
    def msg = s"'$decl' is not accessible from here"
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
    private val localVars = new SymbolTable()
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
      case Some(decl) =>
        if (!decl.isReadable) throw new ResolveException(InaccessibleDeclError(decl))
        else ref.decl = decl
    }

    private def resolveType(expr: Expression): TypeBasic = Typer.getType(expr) match {
      case ta: TypeArray => ta.elementType // let Typer worry about this
      case tb: TypeBasic => tb
    }

    /** Looks up x in the 'localVars' map; if that fails, looks up this.x */
    private def valueLookup(name: String): Option[Decl] = localVars.lookup(name) match {
      case Some(decl) => Some(decl)
      case None => localVars.lookup("this") match {
        case Some(Parameter(_, TypeBasic(className), _, _)) => classes(className).fields.get(name)
        case None => None
      }
    }

    def addLocalVarDecl(decl: Decl) {
      localVars.lookup(decl.name) match {
        case Some(existingDecl) => throw new ResolveException(DuplicateDefinitionError(existingDecl, decl))
        case None =>
      }

      localVars.insert(decl)
    }

    override def postVisit(prog: Program): Unit =
      if (prog.mainMethodDecl.isEmpty) throw new ResolveException(NoMainMethodError())

    override def visit(typ: TypeBasic): Unit = setDecl(typ, classes.get(typ.name).map(_.cls), "type")

    override def visit(method: MethodDecl): Unit = {
      localVars.enterScope()
      method.parameters.foreach(addLocalVarDecl)
      if (method.isStatic) {
        if (method.name != "main") throw new ResolveException(InvalidMainMethodNameError())
        input.mainMethodDecl match {
          case Some(existingMain) =>
            if (!(existingMain eq method)) throw new ResolveException(DuplicateDefinitionError(existingMain, method))
          case None => input.mainMethodDecl = Some(method)
        }
        visit(method.body) // do not visit the arguments since 'String' is not defined in general
      } else {
        super.visit(method)
      }
      localVars.leaveScope()
    }

    override def preVisit(stmt: Block): Unit = localVars.enterScope()
    override def postVisit(stmt: Block, _1: List[Unit]): Unit = localVars.leaveScope()

    /* Visit the initializer *after* the value has been inserted -- it must be visible there.
     *   JLS 14.4.2: "The scope of a local variable declaration in a block (§14.2)
     *   is the rest of the block in which the declaration appears,
     *   starting with its own initializer (§14.4) [...]"
     * 'int x = x;' is only invalid Java because of definite assignment rules, which do not
     * apply to MiniJava. 'int x = (x = 2) * 2' is valid Java and MiniJava. */
    override def preVisit(stmt: LocalVarDeclStatement): Unit = addLocalVarDecl(stmt)

    override def preVisit(expr: Apply): Unit = expr match {
      case Apply("println", Select(Ident("System"), "out") :: args, _) if valueLookup("System") == None =>
        expr.decl = Builtins.SystemOutPrintlnDecl
        expr.arguments = args // throw away fake "this" argument
      case _ =>
    }
    override def postVisit(expr: Apply, _1: List[Unit]): Unit = {
      val name = if (expr.name == "-" && expr.arguments.length == 1) "- (unary)" else expr.name
      operators.get(name) match {
        case Some(op) =>
          expr.decl = op
        case None =>
          if (expr.decl.isEmpty) {
            val qualifierType = resolveType(expr.arguments(0))
            setDecl(expr, classes(qualifierType.name).methods.get(name), "method")
          }
      }
    }

    override def postVisit(expr: Select, _1: Unit): Unit = {
      val thisType = resolveType(expr.qualifier)
      setDecl(expr, classes(thisType.name).fields.get(expr.name), "field")
    }

    override def visit(expr: Ident): Unit = setDecl(expr, valueLookup(expr.name), "value")

    override def visit(expr: ThisLiteral): Unit = {
      // lookup can only be None (for static methods) or the this parameter
      setDecl(expr, valueLookup("this").map(_.asInstanceOf[Parameter]), "value")
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
