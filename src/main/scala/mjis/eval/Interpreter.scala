package mjis.eval

import java.io.{Writer, StringWriter}

import mjis.ast._
import mjis.{Config, Builtins, Severity, AnalysisPhase}

import scala.collection.mutable

class Interpreter(val input: Program) extends AnalysisPhase[String] {
  var writer: Writer = new StringWriter
  override protected def getResult(): String = {
    val main = findMainMethod.get
    stack = mutable.Map[String, (Decl, Instance)]() :: stack
    try {
      evalMethod(main)
    } catch {
      case ex: AbortProgramError =>
        programPrintln(s"=== Aborted Program due to: ${ex.getMessage}\n${ex.getStackTrace.mkString("\n")} ===")
        stack foreach programPrintln
    }
    stack = stack.tail
    writer.toString
  }

  var stack: List[mutable.Map[String, (Decl, Instance)]] = Nil

  def programPrintln(out: Any): Unit = {
    println(s"PROGRAM-OUT: $out")
    writer.write(s"$out\n")
    writer.flush()
  }

  def getValueFromStack(name: String): (Decl, Instance) = {
    var currentList = stack
    var value: Option[(Decl, Instance)] = currentList.head.get(name)
    while (!value.isDefined) {
      currentList = currentList.tail
      value = currentList.head.get(name)
    }
    value.get
  }

  def getThis: Instance =
    stack.head("this")._2

  def getFieldOfThis(name: String): Instance = {
    var thiz: (Decl, Instance) = null
    try {
      thiz = stack.head("this")
      thiz._2.asInstanceOf[Instance].fieldsAndValues(name).asInstanceOf[Instance]
    } catch {
      case ex: NoSuchElementException => throwNRE(s"No field with name $name in this: $thiz")
    }
  }

  val noSuchElementToNullReferenceError: PartialFunction[Throwable, Instance] = {
    case ex: NoSuchElementException => throwNRE(ex.getMessage)
  }
  def throwNRE(details: String) = throw new AbortProgramError(s"null reference error! $details")

  override def success: Boolean = findings.forall(_.severity != Severity.ERROR)

  def findMainMethod = input.classes.iterator.flatMap(_.methods).find(m => m.isStatic && m.name == "main")

  def evalStatement(stmt: Statement): Instance/*.Void*/ =
    stmt match {
      case Block(statements) =>
        statements.foreach(stmt => evalStatement(stmt))
        Instance.Void
      case emptyStmt: EmptyStatement =>
        Instance.Void
      case ifStmt: If =>
        evalIf(ifStmt)
      case whileStmt: While =>
        evalWhile(whileStmt)
      case localVar: LocalVarDeclStatement =>
        stack.head += (localVar.name -> (localVar -> localVar.initializer.map(evalExpression).getOrElse(Instance.uninitializedLocal(localVar.typ))))
        Instance.Void
      case returnStmt: ReturnStatement =>
        val result = returnStmt.returnValue.map(evalExpression).getOrElse(Instance.Void)
        throw new ReturnControlThrowable(result)
      case ExpressionStatement(expr) =>
        evalExpression(expr)
        Instance.Void
    }

  def evalIf(ifStmt: If): Instance =
    if (evalBooleanExpression(ifStmt.condition))
      evalStatement(ifStmt.ifTrue)
    else
      evalStatement(ifStmt.ifFalse)

  def evalWhile(whileStmt: While): Instance = {
    while (evalBooleanExpression(whileStmt.condition)) {
      evalStatement(whileStmt.body)
    }
    Instance.Void
  }

  def evalBooleanExpression(expr: Expression): Boolean = {
    val result = evalExpression(expr)
    if (result == null)
      false
    else
      result.fieldsAndValues("value").asInstanceOf[Boolean]
  }

  def evalIntExpression(expr: Expression): Int = {
    val result = evalExpression(expr)
    if (result == null)
      0
    else
      result.fieldsAndValues("value").asInstanceOf[Int]
  }

  def evalExpression(expr: Expression): Instance =
    expr match {
      case assign:  Assignment => evalAssign(assign)
      case apply:   Apply      => evalApply(apply)
      case newObj:  NewObject  => Instance(newObj.typ.decl)
      case newArr:  NewArray   => evalNewArray(newArr)
      case select:  Select     => evalSelect(select)
      case literal: Literal    => evalLiteral(literal)
    }

  def evalNewArray(newArray: NewArray): Instance =
    Instance.ofArray(newArray.baseType.decl, evalIntExpression(newArray.firstDimSize))

  def evalSelect(select: Select): Instance = {
    select match {
      case Select(qual: Ident, name) =>
        qual.decl match {
          case field: FieldDecl => getFieldOfThis(name)
          case _ =>
            try {
              getValueFromStack(qual.name)._2.fieldsAndValues(name).asInstanceOf[Instance]
            } catch noSuchElementToNullReferenceError
        }
      case Select(thiz: ThisLiteral, name) => getFieldOfThis(name)
      case Select(qual, name) =>
        try {
          evalExpression(qual).fieldsAndValues(name).asInstanceOf[Instance]
        } catch noSuchElementToNullReferenceError
    }
  }

  def evalAssign(assign: Assignment): Instance = {
    // lhs needs to be evaluated before rhs
    lazy val rhs = evalExpression(assign.rhs)
    assign.lhs match {
      case ident: Ident =>
        ident.decl match {
          case field: FieldDecl =>
            getThis.fieldsAndValues(ident.name) = rhs
          case _ =>
            val identDeclName = ident.decl.name
            val rhsDecl = if (rhs != null) rhs.decl else null
            stack.head += ((identDeclName, (rhsDecl, rhs)))
        }
      case Select(qual@Ident(_), name) =>
        qual.decl match {
          case field: FieldDecl =>
            getThis.fieldsAndValues(qual.name).asInstanceOf[Instance].fieldsAndValues(name) = rhs
          case _ =>
            stack.head(qual.name)._2.fieldsAndValues(name) = rhs
        }
      case Select(thiz: ThisLiteral, name) =>
        stack.head(thiz.name)._2.fieldsAndValues(name) = rhs
      case Select(qual, name) =>
        evalExpression(qual).fieldsAndValues(name) = rhs
      case apply: Apply if apply.decl == Builtins.ArrayAccessDecl =>
        val thiz = evalExpression(apply.arguments(0))
        if (this == null) {
          throwNRE(s"array acces -> null")
        } else {
          val index = evalIntExpression(apply.arguments(1))
          val array = thiz.fieldsAndValues("value").asInstanceOf[Array[Any]]
          if (isPrimitive(rhs.decl)) {
            val copy = Instance.zero(rhs.decl)
            copy.fieldsAndValues("value") = rhs.fieldsAndValues("value")
            array(index) = copy
          } else {
            array.apply(index).asInstanceOf[Instance].fieldsAndValues = rhs.fieldsAndValues
          }
        }
    }
    rhs
  }

  def evalLiteral(literal: Literal): Instance = {
    literal match {
      case ident: Ident if ident.decl.isInstanceOf[FieldDecl] =>
        getFieldOfThis(ident.name)
      case ident: Ident => getValueFromStack(ident.name)._2
      case thisLiteral: ThisLiteral => getValueFromStack("this")._2
      case nullLiteral: NullLiteral => Instance.Null
      case intLiteral: IntLiteral => Instance.ofInt(intLiteral.value.toInt)
      case trueLiteral: TrueLiteral => Instance.True
      case falseLiteral: FalseLiteral => Instance.False
    }
  }

  def evalApply(apply: Apply): Instance = {
    import mjis.Builtins._
    apply.decl match {
      case IntAddDecl =>
        val lhs = evalIntExpression(apply.arguments(0))
        val rhs = evalIntExpression(apply.arguments(1))
        val result = lhs + rhs
        Instance.ofInt(result)
      case IntSubDecl =>
        val lhs = evalIntExpression(apply.arguments(0))
        val rhs = evalIntExpression(apply.arguments(1))
        val result = lhs - rhs
        Instance.ofInt(result)
      case IntMinusDecl =>
        val value = evalIntExpression(apply.arguments(0))
        val result = -value
        Instance.ofInt(result)
      case IntMulDecl =>
        val lhs = evalIntExpression(apply.arguments(0))
        val rhs = evalIntExpression(apply.arguments(1))
        val result = lhs * rhs
        Instance.ofInt(result)
      case IntDivDecl =>
        val lhs = evalIntExpression(apply.arguments(0))
        val rhs = evalIntExpression(apply.arguments(1))
        val result = lhs / rhs
        Instance.ofInt(result)
      case IntModDecl =>
        val lhs = evalIntExpression(apply.arguments(0))
        val rhs = evalIntExpression(apply.arguments(1))
        val result = lhs % rhs
        Instance.ofInt(result)
      case IntLessDecl =>
        val lhs = evalIntExpression(apply.arguments(0))
        val rhs = evalIntExpression(apply.arguments(1))
        val result = lhs < rhs
        Instance.ofBoolean(result)
      case IntLessEqualDecl =>
        val lhs = evalIntExpression(apply.arguments(0))
        val rhs = evalIntExpression(apply.arguments(1))
        val result = lhs <= rhs
        Instance.ofBoolean(result)
      case IntGreaterDecl =>
        val lhs = evalIntExpression(apply.arguments(0))
        val rhs = evalIntExpression(apply.arguments(1))
        val result = lhs > rhs
        Instance.ofBoolean(result)
      case IntGreaterEqualDecl =>
        val lhs = evalIntExpression(apply.arguments(0))
        val rhs = evalIntExpression(apply.arguments(1))
        val result = lhs >= rhs
        Instance.ofBoolean(result)
      case BooleanOrDecl =>
        val lhs = evalBooleanExpression(apply.arguments(0))
        lazy val rhs = evalBooleanExpression(apply.arguments(1))
        val result = lhs || rhs
        Instance.ofBoolean(result)
      case BooleanAndDecl =>
        val lhs = evalBooleanExpression(apply.arguments(0))
        lazy val rhs = evalBooleanExpression(apply.arguments(1))
        val result = lhs && rhs
        Instance.ofBoolean(result)
      case BooleanNotDecl =>
        val value = evalBooleanExpression(apply.arguments(0))
        val result = !value
        Instance.ofBoolean(result)
      case SystemOutPrintlnDecl =>
        val value = evalIntExpression(apply.arguments(0))
        programPrintln(value)
        Instance.Void
      case EqualsDecl =>
        val lhs = evalExpression(apply.arguments(0))
        val rhs = evalExpression(apply.arguments(1))
        val result = isEqual(lhs, rhs)
        result
      case UnequalDecl =>
        val lhs = evalExpression(apply.arguments(0))
        val rhs = evalExpression(apply.arguments(1))
        val result = isEqual(lhs, rhs)
        if (result == Instance.True)
          Instance.False
        else
          Instance.True
      case arrAcc@ArrayAccessDecl =>
        val thiz = evalExpression(apply.arguments(0))
        if (this == null) {
          throwNRE(s"$arrAcc is null")
        } else {
          val index = evalIntExpression(apply.arguments(1))
          thiz.fieldsAndValues("value").asInstanceOf[Array[Any]].apply(index).asInstanceOf[Instance]
        }
      case methodDecl: MethodDecl =>
        val args: List[Instance] = apply.arguments.map(evalExpression)
        val params: List[Parameter] = methodDecl.parameters
        val paramsAndArgs = params.zip(args)
        val names: List[String] = methodDecl.parameters.map(_.name)
        val values: List[(String, (Decl, Instance))] = names.zip(paramsAndArgs)
        stack = (mutable.Map[String, (Decl, Instance)]() ++ values) :: stack
        val result = evalMethod(methodDecl)
        stack = stack.tail
        result
    }
  }

  def evalMethod(methodDecl: MethodDecl): Instance = {
    val block = methodDecl.body
    try {
      block.statements.foreach(evalStatement)
      Instance.Void
    } catch {
      case ret: ReturnControlThrowable => ret.instance
    }
  }

  def isEqual(lhs: Instance, rhs: Instance): Instance = {
    //uninitialized field/uninitialized array element/null literal
    if (lhs == null || lhs.decl == null || lhs == Instance.Null)
      if (rhs == null || rhs.decl == null || rhs == Instance.Null)
        return Instance.True
      else
        return Instance.False

    if (isPrimitive(lhs.decl)) {
      if (lhs.fieldsAndValues("value") == rhs.fieldsAndValues("value"))
        return Instance.True
    } else if (lhs eq rhs)
      return Instance.True

    Instance.False
  }

  def isPrimitive(classDecl: ClassDecl) =
    (classDecl eq Builtins.IntDecl) || (classDecl eq Builtins.BooleanDecl)
}
