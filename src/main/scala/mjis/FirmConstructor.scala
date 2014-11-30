package mjis

import java.io.BufferedWriter

import firm.bindings.binding_ircons.op_pin_state
import firm.{ Program => FirmProgram, Ident => _, _ }
import firm.nodes._
import mjis.ast._
import scala.collection.JavaConversions._
import scala.collection.mutable

class FirmConstructor(input: Program) extends Phase[Unit] {
  override protected def getResult(): Unit = {
    transformProgram(input)
  }

  override def dumpResult(writer: BufferedWriter): Unit = {
    FirmProgram.getGraphs.foreach(Dump.dumpGraph(_, "-FirmConstructor"))
  }

  private def transformProgram(prog: Program) = new FirmConstructorVisitor().visit(prog)

  private def mangle(methodName: String) = methodName.replace('.', '_') // TODO

  private class FirmConstructorVisitor extends PlainRecursiveVisitor[Unit, Node, Node]((), null, null) {

    private var graph: Graph = null
    private var constr: Construction = null
    private var lastIndex = 0
    private var declIndex = mutable.Map[Decl, Int]()

    val firmFieldEntity = new mutable.HashMap[FieldDecl, firm.Entity]()
    val firmClassEntity = new mutable.HashMap[ClassDecl, firm.Entity]() {
      override def default(cls: ClassDecl): firm.Entity = {
        val struct = new StructType(cls.name)
        var offset = 0
        var align = 1
        for ((f, i) <- cls.fields.zipWithIndex) {
          val typ = firmType(f.typ)
          val firmField = Entity.createParameterEntity(struct, i, typ)

          // TODO: Align type 'b' like type 'Bu'
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
        new Entity(firm.Program.getGlobalType, cls.name, struct)
      }
    }
    val firmType: mutable.Map[TypeDef, firm.Type] = new mutable.HashMap[TypeDef, firm.Type]() {
      override def default(typ: TypeDef): firm.Type = typ match {
        case Builtins.BooleanType     => new PrimitiveType(Mode.getb)
        case Builtins.IntType         => new PrimitiveType(Mode.getIs)
        case Builtins.ExtendedIntType => new PrimitiveType(Mode.getIu)
        case Builtins.VoidType        => throw new IllegalArgumentException("void doesn't have a runtime type")
        case _                        => new PrimitiveType(Mode.getP)
      }
    }
    private val methodEntity = new mutable.HashMap[MethodDecl, Entity] {
      override def default(decl: MethodDecl) = {
        val paramTypes: Array[Type] = (decl.parameters map { p => firmType(p.typ) }).toArray
        val resultTypes: Array[Type] = decl.typ match {
          case Builtins.VoidType => Array[Type]()
          case t                 => Array[Type](firmType(t))
        }
        val methodType = new MethodType(paramTypes, resultTypes)
        new Entity(FirmProgram.getGlobalType, mangle(decl.name), methodType)
      }
    }

    override def preVisit(method: MethodDecl) = {
      graph = new Graph(methodEntity(method), method.numVars)
      constr = new Construction(graph)
      lastIndex = 0
      declIndex.clear
      // Create a local variable for each parameter. Otherwise, parameters could not be set
      for ((param, index) <- method.parameters.zipWithIndex) {
        newLocalVar(param)
        val proj = constr.newProj(graph.getArgs(), firmType(param.typ).getMode(), index)
        handleLocalVarAssignment(param, proj)
      }
    }

    override def postVisit(method: MethodDecl, _1: Unit, bodyResult: Node): Unit = {
      if (method.typ == Builtins.VoidType) {
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
      }, Mode.getb)
    }

    override def postVisit(expr: IntLiteral): Node = {
      constr.newConst(expr.value.toInt, Mode.getIs) // TODO: what about 2^31?
    }

    override def postVisit(expr: Apply, argumentResults: List[Node]): Node = {
      if (expr.decl.isEmpty) throw new IllegalArgumentException("Unresolved reference")
      expr.decl.get match {
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

        case Builtins.IntLessDecl =>
          constr.newCmp(argumentResults(0), argumentResults(1), Relation.Less)
        case Builtins.IntLessEqualDecl =>
          constr.newCmp(argumentResults(0), argumentResults(1), Relation.LessEqual)
        case Builtins.IntGreaterDecl =>
          constr.newCmp(argumentResults(0), argumentResults(1), Relation.Greater)
        case Builtins.IntGreaterEqualDecl =>
          constr.newCmp(argumentResults(0), argumentResults(1), Relation.GreaterEqual)

        case Builtins.EqualsDecl =>
          constr.newCmp(argumentResults(0), argumentResults(1), Relation.Equal)
        case Builtins.UnequalDecl =>
          val equalNode = constr.newCmp(argumentResults(0), argumentResults(1), Relation.Equal)
          constr.newNot(equalNode, Mode.getb)
        case Builtins.ArrayAccessDecl =>
          val arrayType = Typer.getType((expr.arguments(0))).asInstanceOf[TypeArray]
          var firmArray = firmType(arrayType.elementType)
          // TODO we probably want to cache these types
          for (i <- 0 until arrayType.numDimensions) firmArray = new ArrayType(firmArray)
          val sel = constr.newSel(argumentResults(0), argumentResults(1), firmArray)
          // TODO the lecture slides say we're supposed to use NoMem, but... how?!
          val load = constr.newLoad(constr.getCurrentMem, sel, firmType(arrayType.elementType).getMode)
          constr.setCurrentMem(constr.newProj(load, Mode.getM, firm.nodes.Load.pnM))
          constr.newProj(load, firmType(arrayType.elementType).getMode, firm.nodes.Load.pnRes)
        case decl =>
          call(methodEntity(decl), argumentResults.toArray)
      }
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
      val classType = firmClassEntity(expr.typ.decl.get).getType
      val size = constr.newConst(classType.getSizeBytes, Mode.getIu)
      val num_elems = constr.newConst(1, Mode.getIu)
      call(calloc, Array[Node](num_elems, size))
    }

    override def postVisit(expr: Assignment, lhs: Node, rhs: Node): Node = {
      expr.lhs match {
        case ident: Ident => ident.decl.get match {
          // TODO: Handle field and array assignment
          case local @ (_: Parameter | _: LocalVarDeclStatement) =>
            handleLocalVarAssignment(local.asInstanceOf[TypedDecl], rhs)
        }
        case other => throw new UnsupportedOperationException(s"Unexpected LHS value: $other")
      }
    }
    override def postVisit(ident: Ident): Node = ident.decl.get match {
      case local @ (_: Parameter | _: LocalVarDeclStatement) =>
        constr.getVariable(declIndex(local), firmType(local.asInstanceOf[TypedDecl].typ).getMode())
    }
    override def postVisit(stmt: LocalVarDeclStatement, typResult: Unit, initializerResult: Option[Node]): Node = {
      val localVar = newLocalVar(stmt)
      if (initializerResult.isDefined)
        handleLocalVarAssignment(stmt, initializerResult.get)
      localVar
    }

    private def newLocalVar(decl: TypedDecl): Node = {
      declIndex += decl -> lastIndex
      lastIndex += 1
      constr.getVariable(lastIndex - 1, firmType(decl.typ).getMode)
    }
    private def handleLocalVarAssignment(decl: TypedDecl, rhs: Node): Node = {
      val index = declIndex(decl)
      constr.setVariable(index, rhs)
      constr.getVariable(index, firmType(decl.typ).getMode)
    }
  }

  override def findings: List[Finding] = List()
}
