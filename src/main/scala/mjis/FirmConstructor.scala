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

  private class FirmConstructorVisitor extends PlainRecursiveVisitor[Unit, Unit, Node]((), (), null) {

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
        /* array and class types */
        case _ => new PrimitiveType(Mode.getP)
      }
      firmType += typ -> result
      result
    })

    private def firmArrayType(arrayType: TypeArray): ArrayType = {
      // For FIRM, only one dimension of an array is visible at a time.
      // Multidimensional arrays are just arrays of pointers.
      if (arrayType.numDimensions > 1) new ArrayType(new PrimitiveType(Mode.getP))
      else new ArrayType(firmType(arrayType.elementType))
    }

    private def convbToBu(node: Node) =
      if (node.getMode == Mode.getb)
        constr.newMux(node, constr.newConst(0, Mode.getBu), constr.newConst(1, Mode.getBu), Mode.getBu)
      else node

    private def convBuTob(node: Node) =
      if (node.getMode == Mode.getBu)
        constr.newCmp(node, constr.newConst(1, Mode.getBu), Relation.Equal)
      else node

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

    override def postVisit(method: MethodDecl, _1: Unit, _2: Unit): Unit = {
      if (method.body.isEndReachable) {
        assert(method.returnType == Builtins.VoidType, "reachable end of non-void method")
        graph.getEndBlock.addPred(constr.newReturn(constr.getCurrentMem, Array[Node]()))
      }
      constr.finish()
    }

    override def visit(stmt: mjis.ast.Block): Unit = {
      for (s <- stmt.statements) {
        s.accept(this)
        if (!s.isEndReachable)
          return // skip dead code
      }
    }

    override def visit(stmt: If): Unit = {
      val cond = constr.newCond(convBuTob(stmt.condition.accept(this)))
      val falseX = constr.newProj(cond, Mode.getX, Cond.pnFalse)
      val trueX = constr.newProj(cond, Mode.getX, Cond.pnTrue)

      val follow = constr.newBlock()

      constr.setCurrentBlock(constr.newBlock())
      constr.getCurrentBlock.addPred(trueX)
      stmt.ifTrue.accept(this)
      if (stmt.ifTrue.isEndReachable)
        follow.addPred(constr.newJmp())

      constr.setCurrentBlock(constr.newBlock())
      constr.getCurrentBlock.addPred(falseX)
      stmt.ifFalse.accept(this)
      if (stmt.ifFalse.isEndReachable)
        follow.addPred(constr.newJmp())

      constr.setCurrentBlock(follow)
    }

    override def visit(stmt: While): Unit = {
      val jmp = constr.newJmp()

      val condBlock = constr.newBlock()
      constr.setCurrentBlock(condBlock)
      condBlock.addPred(jmp)
      val cond = constr.newCond(convBuTob(stmt.condition.accept(this)))
      val falseX = constr.newProj(cond, Mode.getX, Cond.pnFalse)
      val trueX = constr.newProj(cond, Mode.getX, Cond.pnTrue)

      constr.setCurrentBlock(constr.newBlock())
      constr.getCurrentBlock.addPred(trueX)
      stmt.body.accept(this)
      if (stmt.body.isEndReachable)
        condBlock.addPred(constr.newJmp())

      constr.setCurrentBlock(constr.newBlock())
      constr.getCurrentBlock.addPred(falseX)
    }

    override def postVisit(stmt: ReturnStatement, exprResult: Option[Node]): Unit = {
      val returnNode = constr.newReturn(constr.getCurrentMem, exprResult match {
        case None            => Array[Node]()
        case Some(valueNode) => Array[Node](convbToBu(valueNode))
      })
      graph.getEndBlock.addPred(returnNode)
    }

    override def preVisit(stmt: LocalVarDeclStatement): Unit = {
      newLocalVar(stmt)
    }

    private def newLocalVar(decl: TypedDecl): Unit = {
      declIndex += decl -> lastIndex
      lastIndex += 1
    }

    private def handleLocalVarAssignment(decl: TypedDecl, rhs: Node): Node = {
      val index = declIndex(decl)
      constr.setVariable(index, rhs)
      constr.getVariable(index, firmType(decl.typ).getMode)
    }

    override def postVisit(stmt: LocalVarDeclStatement, typResult: Unit, initializerResult: Option[Node]): Unit = initializerResult match {
      case Some(node) => handleLocalVarAssignment(stmt, node)
      case None =>
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
          constr.newCmp(argumentResults(0), argumentResults(1), Relation.Less)
        case Builtins.IntLessEqualDecl =>
          constr.newCmp(argumentResults(0), argumentResults(1), Relation.LessEqual)
        case Builtins.IntGreaterDecl =>
          constr.newCmp(argumentResults(0), argumentResults(1), Relation.Greater)
        case Builtins.IntGreaterEqualDecl =>
          constr.newCmp(argumentResults(0), argumentResults(1), Relation.GreaterEqual)

        case Builtins.EqualsDecl =>
          constr.newCmp(convbToBu(argumentResults(0)), convbToBu(argumentResults(1)), Relation.Equal)
        case Builtins.UnequalDecl =>
          val equalNode = constr.newCmp(convbToBu(argumentResults(0)), convbToBu(argumentResults(1)), Relation.Equal)
          constr.newNot(equalNode, Mode.getb)

        case Builtins.ArrayAccessDecl =>
          val sel = createArrayAccess(expr, argumentResults)
          if (expr.isLvalue)
            sel
          else {
            val arrayType = Typer.getType(expr.arguments(0)).asInstanceOf[TypeArray]
            val resultType = firmArrayType(arrayType).getElementType
            createLoad(sel, resultType)
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
      val store = constr.newStore(constr.getCurrentMem, ptr, convbToBu(value))
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
      val call = constr.newCall(constr.getCurrentMem, addr, args.map(convbToBu), methodEntity.getType)
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
      val arrayType = Typer.getType(expr).asInstanceOf[TypeArray]
      val baseType = firmArrayType(arrayType).getElementType
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
      constr.newSel(args(0), args(1), firmArrayType(arrayType))
    }

    override def postVisit(expr: Assignment, lhs: Node, rhs: Node): Node = {
      val rhsVal = convbToBu(rhs)
      expr.lhs match {
        case ident: Ident => ident.decl match {
          case field: FieldDecl =>
            createStore(lhs, rhsVal)
          case local @ (_: Parameter | _: LocalVarDeclStatement) =>
            handleLocalVarAssignment(local, rhsVal)
        }
        case sel: Select =>
          createStore(lhs, rhsVal)
        case arrAccess: Apply =>
          assert(arrAccess.decl eq Builtins.ArrayAccessDecl, "method calls are not valid lvalues")
          createStore(lhs, rhsVal)
        case other => throw new UnsupportedOperationException(s"Unexpected LHS value: $other")
      }
    }

    override def postVisit(ident: Ident): Node = ident.decl match {
      case local @ (_: Parameter | _: LocalVarDeclStatement) =>
        constr.getVariable(declIndex(local), firmType(local.asInstanceOf[TypedDecl].typ).getMode)
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
        constr.getVariable(declIndex(p), firmType(p.typ).getMode)
    }

    override def postVisit(nullLiteral: NullLiteral): Node = constr.newConst(0, Mode.getP)
  }

  override def findings: List[Finding] = List()
}
