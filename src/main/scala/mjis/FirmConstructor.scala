package mjis

import java.io.BufferedWriter

import firm.bindings.binding_ircons.op_pin_state
import firm.{ Program => FirmProgram, Ident => _, _ }
import firm.nodes._
import mjis.ast._
import mjis.util.FirmDumpHelper
import scala.collection.JavaConversions._
import scala.collection.mutable

class FirmConstructor(input: Program) extends Phase[Unit] {
  override protected def getResult(): Unit = {
    transformProgram(input)
  }

  override def dumpResult(writer: BufferedWriter): Unit = {
    FirmProgram.getGraphs.foreach(FirmDumpHelper.dumpGraph(_, "-FirmConstructor"))
  }

  private def transformProgram(prog: Program) = new FirmConstructorVisitor().visit(prog)

  private def mangle(method: MethodDecl, cls: ClassDecl) = {
    if (method.isStatic)
      method.name.replace('.', '_')
    else
      "_" + cls.name.length.toString + cls.name + "_" + method.name
  }

  private class FirmConstructorVisitor extends TailRecursiveVisitor[Unit, Node, Node]((), null, null) {

    private var graph: Graph = null
    private var constr: Construction = null
    private var lastIndex = 0
    private var declIndex = mutable.Map[Decl, Int]()

    val firmClassEntity = new mutable.HashMap[ClassDecl, firm.Entity]()
    val firmFieldEntity = new mutable.HashMap[FieldDecl, firm.Entity]()
    val firmMethodEntity = new mutable.HashMap[MethodDecl, firm.Entity]()

    val firmType: mutable.Map[TypeDef, firm.Type] = new mutable.HashMap().withDefault(typ => {
      val result = typ match {
        case Builtins.BooleanType => new PrimitiveType(Mode.getBu)
        case Builtins.IntType => new PrimitiveType(Mode.getIs)
        case Builtins.ExtendedIntType => new PrimitiveType(Mode.getIu)
        case Builtins.VoidType => throw new IllegalArgumentException("void doesn't have a runtime type")
        case array: ast.TypeArray     =>
          var firmArray = firmType(array.elementType)
          for (i <- 0 until array.numDimensions) firmArray = new ArrayType(firmArray)
          firmArray
        case _ => new PrimitiveType(Mode.getP)
      }
      firmType += typ -> result
      result
    })

    private def convToBu(node: Node) =
      constr.newMux(node, constr.newConst(0, Mode.getBu), constr.newConst(1, Mode.getBu), Mode.getBu)

    private def createMethodEntity(cls: ClassDecl, method: MethodDecl) = {
      val paramTypes: Array[Type] = (method.parameters map { p => firmType(p.typ) }).toArray
      val resultTypes: Array[Type] = method.returnType match {
        case Builtins.VoidType => Array[Type]()
        case t                 => Array[Type](firmType(t))
      }
      val methodType = new MethodType(paramTypes, resultTypes)
      new Entity(FirmProgram.getGlobalType, mangle(method, cls), methodType)
    }

    override def preVisit(program: Program): Unit = {
      // Create class, field and method entities
      program.classes.foreach(cls => {
        val struct = new StructType(cls.name)
        var offset = 0
        var align = 1
        for ((f, i) <- cls.fields.zipWithIndex) {
          val typ = firmType(f.typ)
          val firmField = Entity.createParameterEntity(struct, i, typ)
          firmFieldEntity += f -> firmField

          val fieldAlign = typ.getAlignmentBytes
          if (offset % fieldAlign > 0)
            offset += fieldAlign - offset % fieldAlign

          firmField.setOffset(offset)
          offset += typ.getSizeBytes

          align = math.max(align, fieldAlign) // powers of two
        }
        struct.setSizeBytes(offset)
        struct.setAlignmentBytes(align)
        struct.finishLayout()
        firmClassEntity += cls -> new Entity(firm.Program.getGlobalType, cls.name, struct)

        cls.methods.foreach(method => firmMethodEntity += method -> createMethodEntity(cls, method))
      })

      // Special case: System.out.println; it is the only MethodDecl in Builtins which is not an operator
      firmMethodEntity += Builtins.SystemOutPrintlnDecl -> createMethodEntity(null, Builtins.SystemOutPrintlnDecl)
    }

    override def preVisit(method: MethodDecl) = {
      graph = new Graph(firmMethodEntity(method), method.numVars)
      constr = new Construction(graph)
      lastIndex = 0
      declIndex.clear()
      // Create a local variable for each parameter. Otherwise, parameters could not be set
      for ((param, index) <- method.parameters.zipWithIndex) {
        newLocalVar(param)
        val proj = constr.newProj(graph.getArgs, firmType(param.typ).getMode, index)
        handleLocalVarAssignment(param, proj)
      }
    }

    override def postVisit(method: MethodDecl, _1: Unit, bodyResult: Node): Unit = {
      if (method.returnType == Builtins.VoidType) {
        graph.getEndBlock.addPred(constr.newReturn(constr.getCurrentMem, Array[Node]()))
      }
      constr.finish()
    }

    override def postVisit(stmt: ReturnStatement, exprResult: Option[Node]): Node = {
      val returnNode = constr.newReturn(constr.getCurrentMem, exprResult match {
        case None            => Array[Node]()
        case Some(valueNode) => Array[Node](valueNode)
      })
      graph.getEndBlock.addPred(returnNode)
      returnNode
    }

    override def postVisit(expr: BooleanLiteral): Node = {
      constr.newConst(expr match {
        case TrueLiteral()  => 1
        case FalseLiteral() => 0
      }, Mode.getBu)
    }

    override def postVisit(expr: IntLiteral): Node = {
      Typer.getType(expr) match {
        case Builtins.IntType => constr.newConst(expr.value.toInt, Mode.getIs)
        case Builtins.ExtendedIntType => constr.newConst(new TargetValue(expr.value.toLong, Mode.getIu))
        case _ => throw new IllegalArgumentException("Invalid type for IntLiteral")
      }
    }

    override def postVisit(expr: Apply, argumentResults: List[Node]): Node = {
      expr.decl match {
        case Builtins.IntAddDecl =>
          constr.newAdd(argumentResults(0), argumentResults(1), Mode.getIs)
        case Builtins.IntSubDecl =>
          constr.newSub(argumentResults(0), argumentResults(1), Mode.getIs)
        case Builtins.IntMulDecl =>
          constr.newMul(argumentResults(0), argumentResults(1), Mode.getIs)
        case Builtins.IntDivDecl =>
          val divNode = constr.newDiv(constr.getCurrentMem, argumentResults(0), argumentResults(1),
            Mode.getIs, op_pin_state.op_pin_state_floats)
          constr.setCurrentMem(constr.newProj(divNode, Mode.getM, firm.nodes.Div.pnM))
          constr.newProj(divNode, Mode.getIs, firm.nodes.Div.pnRes)
        case Builtins.IntModDecl =>
          val modNode = constr.newMod(constr.getCurrentMem, argumentResults(0), argumentResults(1),
            Mode.getIs, op_pin_state.op_pin_state_floats)
          constr.setCurrentMem(constr.newProj(modNode, Mode.getM, firm.nodes.Mod.pnM))
          constr.newProj(modNode, Mode.getIs, firm.nodes.Mod.pnRes)

        case Builtins.ExtendedIntMinusDecl =>
          val minusNode = constr.newMinus(argumentResults(0), argumentResults(0).getMode)
          if (argumentResults(0).getMode == Mode.getIu) {
            constr.newConv(minusNode, Mode.getIs)
          } else {
            minusNode
          }

        case Builtins.IntLessDecl =>
          convToBu(constr.newCmp(argumentResults(0), argumentResults(1), Relation.Less))
        case Builtins.IntLessEqualDecl =>
          convToBu(constr.newCmp(argumentResults(0), argumentResults(1), Relation.LessEqual))
        case Builtins.IntGreaterDecl =>
          convToBu(constr.newCmp(argumentResults(0), argumentResults(1), Relation.Greater))
        case Builtins.IntGreaterEqualDecl =>
          convToBu(constr.newCmp(argumentResults(0), argumentResults(1), Relation.GreaterEqual))

        case Builtins.EqualsDecl =>
          convToBu(constr.newCmp(argumentResults(0), argumentResults(1), Relation.Equal))
        case Builtins.UnequalDecl =>
          val equalNode = constr.newCmp(argumentResults(0), argumentResults(1), Relation.Equal)
          convToBu(constr.newNot(equalNode, Mode.getb))

        case Builtins.ArrayAccessDecl =>
          val sel = createArrayAccess(expr, argumentResults)
          if (expr.isLvalue)
            sel
          else {
            val arrayType = Typer.getType(expr.arguments(0)).asInstanceOf[TypeArray]
            val firmArray = firmType(arrayType.elementType)
            createLoad(sel, firmArray)
          }

        case decl =>
          call(firmMethodEntity(decl), argumentResults.toArray)
      }
    }

    private def createLoad(ptr: Node, typ: Type): Node = {
      // FIRM doesn't have a proper concept for array loads, so we need to catch that case and special case it
      val mode = if (typ.isInstanceOf[ArrayType]) Mode.getP else typ.getMode
      val load = constr.newLoad(constr.getCurrentMem, ptr, mode, typ)
      constr.setCurrentMem(constr.newProj(load, Mode.getM, firm.nodes.Load.pnM))
      constr.newProj(load, mode, firm.nodes.Load.pnRes)
    }

    private def createStore(ptr: Node, value: Node): Node = {
      val store = constr.newStore(constr.getCurrentMem, ptr, value)
      val newMem = constr.newProj(store, Mode.getM, firm.nodes.Store.pnM)
      constr.setCurrentMem(newMem)
      value
    }

    override def postVisit(expr: Select, qualifier: Node): Node = {
      assert(qualifier.getMode == Mode.getP, qualifier.getMode)
      val fieldEntity = firmFieldEntity(expr.decl)
      val member = constr.newMember(qualifier, fieldEntity)
      if (expr.isLvalue)
        member
      else
        createLoad(member, fieldEntity.getType)
    }

    private val callocType = {
      val Iu = firmType(Builtins.ExtendedIntType)
      val params: Array[Type] = List(Iu, Iu).toArray
      // we can't construct a void*, so we take the next best thing: char*
      val result: Array[Type] = Array(new PointerType(new PrimitiveType(Mode.getBu)))
      new MethodType(params, result)
    }
    private val calloc = new Entity(FirmProgram.getGlobalType, "calloc", callocType)

    private def call(methodEntity: Entity, args: Array[Node]): Node = {
      val methodType = methodEntity.getType.asInstanceOf[MethodType]
      val addr = constr.newAddress(methodEntity)
      val call = constr.newCall(constr.getCurrentMem, addr, args, methodEntity.getType)
      constr.setCurrentMem(constr.newProj(call, Mode.getM, firm.nodes.Call.pnM))
      // libFirm *really* doesn't like us trying to access void
      if (methodType.getNRess > 0) {
        val res = constr.newProj(call, Mode.getT, firm.nodes.Call.pnTResult)
        constr.newProj(
          res,
          methodType.getResType(0).getMode,
          0)
      } else call
    }

    override def postVisit(expr: NewArray, _1: Unit, firstDimSize: Node): Node = {
      val baseType = firmType(expr.typ)
      val size = constr.newConst(baseType.getSizeBytes, Mode.getIu)
      val elems = constr.newConv(firstDimSize, Mode.getIu)
      call(calloc, Array[Node](elems, size))
    }

    override def postVisit(expr: NewObject, _1: Unit): Node = {
      val classType = firmClassEntity(expr.typ.decl).getType
      val size = constr.newConst(classType.getSizeBytes, Mode.getIu)
      val num_elems = constr.newConst(1, Mode.getIu)
      call(calloc, Array[Node](num_elems, size))
    }

    private def createArrayAccess(expr: Apply, args: List[Node]): Node = {
      assert(expr.decl eq Builtins.ArrayAccessDecl, "method calls may not be used as lvalues")
      val arrayType = Typer.getType(expr.arguments(0)).asInstanceOf[TypeArray]
      val firmArray = firmType(arrayType)
      constr.newSel(args(0), args(1), firmArray)
    }

    override def postVisit(expr: Assignment, lhs: Node, rhs: Node): Node = {
      expr.lhs match {
        case ident: Ident => ident.decl match {
          case field: FieldDecl =>
            createStore(lhs, rhs)
          case local @ (_: Parameter | _: LocalVarDeclStatement) =>
            handleLocalVarAssignment(local, rhs)
        }
        case sel: Select =>
          createStore(lhs, rhs)
        case arrAccess: Apply =>
          assert(arrAccess.decl eq Builtins.ArrayAccessDecl, "method calls are not valid lvalues")
          createStore(lhs, rhs)
        case other => throw new UnsupportedOperationException(s"Unexpected LHS value: $other")
      }
    }

    override def postVisit(ident: Ident): Node = ident.decl match {
      case local @ (_: Parameter | _: LocalVarDeclStatement) =>
        getVariable(declIndex(local), local.asInstanceOf[TypedDecl])
      case field: FieldDecl =>
        val fieldEntity = firmFieldEntity(field)
        val thisPtr = constr.getVariable(0, Mode.getP)
        val member = constr.newMember(thisPtr, fieldEntity)
        if (ident.isLvalue)
          member
        else
          createLoad(member, fieldEntity.getType)
    }

    override def postVisit(thisLit: ThisLiteral): Node = thisLit.decl match {
      case p: Parameter =>
        getVariable(declIndex(p), p)
    }

    override def preVisit(stmt: LocalVarDeclStatement): Unit = {
      newLocalVar(stmt)
    }

    override def postVisit(stmt: LocalVarDeclStatement, typResult: Unit, initializerResult: Option[Node]): Node = {
      if (initializerResult.isDefined)
        handleLocalVarAssignment(stmt, initializerResult.get)
      constr.getVariable(declIndex(stmt), firmType(stmt.typ).getMode)
    }

    // array-safe wrapper around constr.getVariable
    private def getVariable(index: Int, decl: TypedDecl): Node = {
      val fType = firmType(decl.typ)
      val m = if (fType.isInstanceOf[ArrayType]) Mode.getP else fType.getMode
      constr.getVariable(index, m)
    }

    private def newLocalVar(decl: TypedDecl): Unit = {
      declIndex += decl -> lastIndex
      lastIndex += 1
      getVariable(lastIndex - 1, decl)
    }
    private def handleLocalVarAssignment(decl: TypedDecl, rhs: Node): Node = {
      val index = declIndex(decl)
      constr.setVariable(index, rhs)
      getVariable(index, decl)
    }
 }

  override def findings: List[Finding] = List()
}
