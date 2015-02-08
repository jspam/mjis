package mjis

import firm.Firm
import mjis.asm._
import mjis.asm.AMD64Registers._
import mjis.asm.OperandSpec._
import mjis.util.Digraph
import org.scalatest._
import mjis.CompilerTestMatchers._

class RegisterAllocatorTest extends FlatSpec with Matchers with BeforeAndAfter {

  before {
    Firm.init()
  }

  after {
    Firm.finish()
  }

  def addBlocks(fun: AsmFunction, cfEdges: (AsmBasicBlock, AsmBasicBlock)*): AsmFunction = {
    for ((pred, succ) <- cfEdges) {
      if (!fun.basicBlocks.contains(pred)) fun.basicBlocks :+= pred
      if (!fun.basicBlocks.contains(succ)) fun.basicBlocks :+= succ
      pred.successors += succ
      succ.predecessors += Some(pred)
    }

    fun.prologue.successors += fun.basicBlocks.head
    fun.basicBlocks.head.predecessors += Some(fun.prologue)

    fun.epilogue.predecessors += Some(fun.basicBlocks.last)
    fun.basicBlocks.last.successors += fun.epilogue
    fun.epilogue.instructions += Ret()

    fun.basicBlocks = (fun.prologue :: fun.basicBlocks) :+ fun.epilogue
    fun
  }

  def buildFunction(name: String, cfEdges: (AsmBasicBlock, AsmBasicBlock)*): AsmFunction = {
    val fun = new AsmFunction(name)
    addBlocks(fun, cfEdges:_*)
  }

  def buildFunction(cfEdges: (AsmBasicBlock, AsmBasicBlock)*): AsmFunction = buildFunction("test", cfEdges:_*)

  def setCustomRet(fun: AsmFunction, ret: Instruction): Unit = {
    fun.epilogue.instructions.clear()
    fun.epilogue.instructions += ret
  }

  "The register allocator" should "allocate registers for a simple function" in {
    /* function(i) => return i+2 */
    Seq(
      Mov(RegisterOperand(RAX, 4), RegisterOperand(10, 4)),
      Add(ConstOperand(2, 4), RegisterOperand(10, 4)),
      Mov(RegisterOperand(10, 4), RegisterOperand(RDX, 4))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RDX, RAX), expected =
      """  movl %eax, %eax
        |  addl $2, %eax
        |  movl %eax, %edx""")
  }

  it should "avoid blocked registers" in {
    Seq(
      Mov(ConstOperand(0, 4), RegisterOperand(10, 4)),
      Call(LabelOperand("_foobar"), Seq()), // blocks EAX
      Mov(RegisterOperand(10, 4), AddressOperand(base = Some(RegisterOperand(RSP, 8)), sizeBytes = 4))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RAX, RBX), callerSaveRegs = Set(RAX), expected =
      """  movl $0, %ebx
        |  call _foobar
        |  movl %ebx, (%rsp)""")
  }

  it should "spill register contents if necessary" in {
    Seq(
      Mov(ConstOperand(0, 4), RegisterOperand(10, 8)),
      Mov(ConstOperand(41, 4), AddressOperand(base = Some(RegisterOperand(10, 8)), sizeBytes = 4)),
      Mov(ConstOperand(0, 4), RegisterOperand(11, 8)),
      Mov(ConstOperand(42, 4), AddressOperand(base = Some(RegisterOperand(10, 8)), sizeBytes = 4)),
      Mov(ConstOperand(43, 4), AddressOperand(base = Some(RegisterOperand(11, 8)), sizeBytes = 4))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RAX) /* we only have one register */, expected =
      """  movq $0, %rax      # REG10 => RAX
        |  movl $41, (%rax)
        |  movq %rax, 8(%rsp)  # spill REG10
        |  movq $0, %rax      # REG11 => RAX
        |  movq %rax, (%rsp)   # spill REG11
        |  movq 8(%rsp), %rax # reload REG10 => RAX
        |  movl $42, (%rax)
        |  movq (%rsp), %rax  # reload REG11 => RAX
        |  movl $43, (%rax)""")
  }

  it should "spill register contents into SSE registers if available" in {
    Seq(
      Mov(ConstOperand(0, 4), RegisterOperand(10, 8)),
      Mov(ConstOperand(41, 4), AddressOperand(base = Some(RegisterOperand(10, 8)), sizeBytes = 4)),
      Mov(ConstOperand(0, 4), RegisterOperand(11, 8)),
      Mov(ConstOperand(42, 4), AddressOperand(base = Some(RegisterOperand(10, 8)), sizeBytes = 4)),
      Mov(ConstOperand(43, 4), AddressOperand(base = Some(RegisterOperand(11, 8)), sizeBytes = 4))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RAX) /* we only have one register */, sseRegs = Set(XMM0, XMM1), expected =
      """  movq $0, %rax      # REG10 => RAX
        |  movl $41, (%rax)
        |  movq %rax, %xmm0   # spill REG10
        |  movq $0, %rax      # REG11 => RAX
        |  movq %rax, %xmm1    # spill REG11
        |  movq %xmm0, %rax   # reload REG10 => RAX
        |  movl $42, (%rax)
        |  movq %xmm1, %rax    # reload REG11 => RAX
        |  movl $43, (%rax)""")
  }

  it should "not spill 8-bit registers into a SSE register" in {
    val b1 = new AsmBasicBlock(0)
    b1.instructions ++= Seq(
      Mov(ConstOperand(0, 1), RegisterOperand(10, 1)),
      Call(LabelOperand("System_out_println"), Seq()),
      Mov(RegisterOperand(10, 1), RegisterOperand(RAX, 1)),
      Mov(RegisterOperand(10, 1), RegisterOperand(RDX, 1))
    )
    val b2 = new AsmBasicBlock(1)
    val main = buildFunction("__main", (b1, b2))
    val prog = new AsmProgram(List(main), new Digraph[AsmFunction](Map(main -> Seq())))

    prog should succeedAllocatingRegistersForProgramWith(Seq(RAX, RDX), callerSaveRegs = Set(RAX, RDX), sseRegs = Set(XMM0), expected =
      """__main:
        |  subq $8, %rsp
        |.L0:
        |  movb $0, %al
        |  movb %al, 7(%rsp)
        |  call System_out_println
        |  movb 7(%rsp), %dl
        |  movb %dl, %al
        |  movb %dl, %dl
        |.L1:
        |  addq $8, %rsp
        |  ret""")
  }

  it should "respect liveness intervals ending at the following instruction when reloading" in {
    Seq(
      Mov(ConstOperand(0, 8), RegisterOperand(10, 8)), // gets assigned EDX
      Call(LabelOperand("_foobar"), Seq()),            // blocks both registers => EDX gets spilled
      Mov(ConstOperand(2, 4), RegisterOperand(11, 4)), // gets assigned EDX (reg10 is spilled!)
      // REG10 is reloaded before this instruction, but may not use EDX although its liveness interval ends here.
      Mov(ConstOperand(3, 4), AddressOperand(base = Some(RegisterOperand(10, 8)),
        indexAndScale = Some((RegisterOperand(11, 4), 1)), sizeBytes = 4)),
      Mov(ConstOperand(42, 4), AddressOperand(base = Some(RegisterOperand(10, 8)), sizeBytes = 4))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RDX, RCX), callerSaveRegs = Set(RDX, RCX), expected =
      """  movq $0, %rdx
        |  movq %rdx, (%rsp) # spill
        |  call _foobar
        |  movl $2, %edx
        |  movq (%rsp), %rcx # reload
        |  movl $3, (%rcx,%rdx)
        |  movl $42, (%rcx)""")
  }

  it should "handle Phi functions for non-existing predecessors" in {
    val b1 = new AsmBasicBlock(1)

    val b2 = new AsmBasicBlock(2)
    b2.phis += Phi(Seq(ConstOperand(0, 1), ConstOperand(1, 1)), RegisterOperand(10, 1))
    b2.instructions += Mov(RegisterOperand(10, 1), RegisterOperand(RAX, 1))

    val fun = buildFunction((b1, b2))
    setCustomRet(fun, Ret(1))

    fun should succeedAllocatingRegistersWith(Seq(RAX), expected =
      """.L1:
        |.L2:
        |  movb $0, %al  # from resolving phi
        |  movb %al, %al # reg 10 has been assigned register al
        |  ret""")
  }

  it should "leave rsp alone" in {
    Seq(
      Mov(ConstOperand(0, 4), RegisterOperand(10, 4)),
      Mov(ConstOperand(0, 8), RegisterOperand(11, 8)),
      Mov(ConstOperand(2, 4), RegisterOperand(RSP, 8)),
      Mov(ConstOperand(0, 4), AddressOperand(base = Some(RegisterOperand(11, 8)), indexAndScale = Some((RegisterOperand(10, 4), 1)), sizeBytes = 4)),
      Mov(RegisterOperand(RSP, 8), RegisterOperand(11, 8))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RAX, RBX), expected =
      """  movl $0, %eax
        |  movq $0, %rbx
        |  movq $2, %rsp
        |  movl $0, (%rbx,%rax)
        |  movq %rsp, %rbx""")
  }

  it should "select the same spilling position when the same register is spilled twice" in {
    Seq(
      Mov(ConstOperand(0, 4), RegisterOperand(10, 4)),
      Call(LabelOperand("_foobar"), Seq()),
      Mov(ConstOperand(0, 4), AddressOperand(base = Some(RegisterOperand(10, 4)), sizeBytes = 4)),
      Call(LabelOperand("_foobar"), Seq()),
      Mov(ConstOperand(0, 4), AddressOperand(base = Some(RegisterOperand(10, 4)), sizeBytes = 4))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RCX), callerSaveRegs = Set(RCX), expected =
      """  movl $0, %ecx
        |  movl %ecx, 4(%rsp)  # spill 1
        |  call _foobar
        |  movl 4(%rsp), %ecx  # reload 1
        |  movl $0, (%ecx)
        |  movl %ecx, 4(%rsp)  # spill 2
        |  call _foobar
        |  movl 4(%rsp), %ecx  # reload 2
        |  movl $0, (%ecx)""")
  }

  it should "convert existing ActivationRecordOperands" in {
    Seq(
      Mov(ActivationRecordOperand(0, 8), RegisterOperand(RAX, 8))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RAX), expected =
      """  movq (%rsp), %rax""")
  }

  it should "insert correct moves when an interval is split at a block boundary" in {
    val b0 = new AsmBasicBlock(0)
    b0.instructions += Mov(ConstOperand(4, 4), RegisterOperand(10, 4))

    val b1 = new AsmBasicBlock(1)
    b1.instructions += Call(LabelOperand("_foobar"), Seq())

    val b2 = new AsmBasicBlock(2)

    val b3 = new AsmBasicBlock(3)
    b3.phis += Phi(Seq(ConstOperand(1, 4), ConstOperand(8, 4)), RegisterOperand(11, 4))
    b3.instructions += Mov(RegisterOperand(11, 4), AddressOperand(base = Some(RegisterOperand(RBX, 8)), sizeBytes = 4))
    b3.instructions += Mov(RegisterOperand(10, 4), AddressOperand(base = Some(RegisterOperand(RBX, 8)), sizeBytes = 4))

    val fun = buildFunction((b0, b1), (b0, b2), (b1, b3), (b2, b3))

    fun should succeedAllocatingRegistersWith(Seq(RAX), expected =
      """  subq $8, %rsp
        |.L0:
        |  movl $4, %eax
        |  movl %eax, 4(%rsp)
        |.L1:
        |  call _foobar
        |  movl $1, %eax
        |.L2:
        |  movl $8, %eax
        |.L3:
        |  movl %eax, (%rbx)
        |  movl 4(%rsp), %eax
        |  movl %eax, (%rbx)
        |  addq $8, %rsp
        |  ret""")
  }

  it should "use the same register for multiple connected intervals if possible" in {
    Seq(
      Mov(ConstOperand(0, 4), RegisterOperand(10, 4)),  // gets assigned EBX because EAX is blocked
      Call(LabelOperand("_foobar"), Seq()),             // blocks EAX
      Mov(RegisterOperand(10, 4), RegisterOperand(11, 4)), // EAX is available for REG11 here, but EBX should be taken
      Mov(RegisterOperand(11, 4), AddressOperand(base = Some(RegisterOperand(RSP, 8)), sizeBytes = 4))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RAX, RBX), callerSaveRegs = Set(RAX), expected =
      """  movl $0, %ebx
        |  call _foobar
        |  movl %ebx, %ebx
        |  movl %ebx, (%rsp)""")
  }

  it should "automatically select a temporary register for phi generation" in {
    val b0 = new AsmBasicBlock(0)
    val b1 = new AsmBasicBlock(1)
    b1.phis += Phi(Seq(ConstOperand(2, 4), RegisterOperand(10, 4)), RegisterOperand(11, 4))
    b1.phis += Phi(Seq(ConstOperand(1, 4), RegisterOperand(11, 4)), RegisterOperand(10, 4))
    val b2 = new AsmBasicBlock(2)
    val b3 = new AsmBasicBlock(3)

    buildFunction((b0, b1), (b1, b2), (b2, b1), (b1, b3)) should succeedAllocatingRegistersWith(Seq(RAX, RCX, RDX), expected =
      """.L0:
        |  movl $2, %eax
        |  movl $1, %edx
        |.L1:
        |.L2:
        |  movl %eax, %ecx
        |  movl %edx, %eax
        |  movl %ecx, %edx
        |.L3:
        |  ret""")
  }

  it should "spill a register if there aren't enough temporary registers available for phi generation" in {
    val b0 = new AsmBasicBlock(0)
    b0.instructions += Mov(ConstOperand(4, 4), RegisterOperand(20, 4))
    val b1 = new AsmBasicBlock(1)
    b1.phis += Phi(Seq(ConstOperand(2, 4), RegisterOperand(10, 4)), RegisterOperand(11, 4))
    b1.phis += Phi(Seq(ConstOperand(1, 4), RegisterOperand(11, 4)), RegisterOperand(10, 4))
    val b2 = new AsmBasicBlock(2)
    val b3 = new AsmBasicBlock(3)
    b3.instructions += Mov(RegisterOperand(20, 4), RegisterOperand(RAX, 4))

    buildFunction((b0, b1), (b1, b2), (b2, b1), (b1, b3)) should succeedAllocatingRegistersWith(Seq(RAX, RCX, RDX), expected =
      """  subq $8, %rsp
        |.L0:
        |  movl $4, %eax
        |  movl $2, %edx
        |  movl $1, %ecx
        |.L1:
        |.L2:
        |  movl %eax, 4(%rsp)
        |  movl %edx, %eax
        |  movl %ecx, %edx
        |  movl %eax, %ecx
        |  movl 4(%rsp), %eax
        |.L3:
        |  movl %eax, %eax
        |  addq $8, %rsp
        |  ret""")
  }

  it should "modify the phi moves when a participating register has to be spilled" in {
    val b0 = new AsmBasicBlock(0)
    val b1 = new AsmBasicBlock(1)
    b1.phis += Phi(Seq(ConstOperand(2, 4), RegisterOperand(10, 4)), RegisterOperand(11, 4))
    b1.phis += Phi(Seq(ConstOperand(1, 4), RegisterOperand(11, 4)), RegisterOperand(10, 4))
    val b2 = new AsmBasicBlock(2)
    val b3 = new AsmBasicBlock(3)

    buildFunction((b0, b1), (b1, b2), (b2, b1), (b1, b3)) should succeedAllocatingRegistersWith(Seq(RAX, RCX), expected =
      """  subq $16, %rsp
        |.L0:
        |  movl $2, %eax
        |  movl $1, %ecx
        |.L1:
        |.L2:
        |  movl %eax, (%rsp)  # spill
        |  movl %ecx, 8(%rsp) # spill
        |  movl (%rsp), %ecx
        |  movl 8(%rsp), %eax
        |  movl %eax, (%rsp)
        |  movl %ecx, 8(%rsp)
        |  movl (%rsp), %eax  # reload
        |  movl 8(%rsp), %ecx # reload
        |.L3:
        |  addq $16, %rsp
        |  ret""")
  }

  it should "spill registers that live through a loop before the loop" in {
    val before = new AsmBasicBlock(0)
    before.instructions += Mov(ConstOperand(0, 8), RegisterOperand(10, 8))

    val header = new AsmBasicBlock(1)

    val body = new AsmBasicBlock(2)
    body.instructions += Call(LabelOperand("_foobar"), Seq())

    val after = new AsmBasicBlock(3)
    after.instructions += Mov(RegisterOperand(10, 8),
      AddressOperand(base = Some(RegisterOperand(10, 8)), sizeBytes = 8))

    val fun = buildFunction((before, header), (header, body), (body, header), (header, after))

    fun should succeedAllocatingRegistersWith(Seq(RAX), callerSaveRegs = Set(RAX), expected =
      """  subq $8, %rsp
        |.L0:
        |  movq $0, %rax
        |  movq %rax, (%rsp)
        |.L1:
        |.L2:
        |  call _foobar
        |.L3:
        |  movq (%rsp), %rax
        |  movq %rax, (%rax)
        |  addq $8, %rsp
        |  ret""")
  }

  it should "reload registers that are needed in a loop before the loop" in {
    val before = new AsmBasicBlock(0)
    before.instructions += Mov(ConstOperand(0, 4), RegisterOperand(10, 4))
    before.instructions += Call(LabelOperand("_foobar"), Seq())

    val header = new AsmBasicBlock(1)

    val body = new AsmBasicBlock(2)
    body.instructions += Mov(RegisterOperand(10, 4), AddressOperand(base = Some(RegisterOperand(RSP, 8)), sizeBytes = 4))

    val after = new AsmBasicBlock(3)

    val fun = buildFunction((before, header), (header, body), (body, header), (header, after))

    fun should succeedAllocatingRegistersWith(Seq(RAX), callerSaveRegs = Set(RAX), expected =
      """  subq $8, %rsp
        |.L0:
        |  movl $0, %eax
        |  movl %eax, 4(%rsp)
        |  call _foobar
        |  movl 4(%rsp), %eax
        |.L1:
        |.L2:
        |  movl %eax, (%rsp)
        |.L3:
        |  addq $8, %rsp
        |  ret""")
  }

  it should "not save caller-save registers that aren't used in the called function" in {
    val b11 = new AsmBasicBlock(0)
    b11.instructions += Mov(RegisterOperand(RDI, 4), RegisterOperand(RAX, 4))
    val b12 = new AsmBasicBlock(1)
    val fun1 = buildFunction("fun1", (b11, b12))

    val b21 = new AsmBasicBlock(2)
    b21.instructions ++= Seq(
      Mov(ConstOperand(0, 4), RegisterOperand(10, 4)),
      Call(LabelOperand("fun1"), Seq()),
      Mov(RegisterOperand(10, 4), RegisterOperand(RAX, 4))
    )
    val b22 = new AsmBasicBlock(3)
    val main = buildFunction("__main", (b21, b22))

    val prog = new AsmProgram(List(fun1, main), new Digraph[AsmFunction](Map(main -> Seq(fun1), fun1 -> Seq())))

    prog should succeedAllocatingRegistersForProgramWith(Seq(RAX, RDI, RSI), callerSaveRegs = Set(RAX, RDI, RSI), expected =
      """fun1:
        |.L0:
        |  movl %edi, %eax
        |.L1:
        |  ret
        |
        |__main:
        |.L2:
        |  movl $0, %esi    # esi is a caller-save register, but not needed by fun1
        |  call fun1
        |  movl %esi, %eax
        |.L3:
        |  ret""")
  }

  it should "save all caller-save registers when calling an external function" in {
    val b1 = new AsmBasicBlock(2)
    b1.instructions ++= Seq(
      Mov(ConstOperand(0, 4), RegisterOperand(10, 4)),
      Call(LabelOperand("calloc"), Seq()),
      Mov(RegisterOperand(10, 4), RegisterOperand(RAX, 4))
    )
    val b2 = new AsmBasicBlock(3)
    val main = buildFunction("__main", (b1, b2))

    val prog = new AsmProgram(List(main), new Digraph[AsmFunction](Map(main -> Seq())))

    prog should succeedAllocatingRegistersForProgramWith(Seq(RAX), callerSaveRegs = Set(RAX), expected =
      """__main:
        |  subq $8, %rsp
        |.L0:
        |  movl $0, %eax
        |  movl %eax, 4(%rsp)
        |  call calloc
        |  movl 4(%rsp), %eax
        |.L3:
        |  addq $8, %rsp
        |  ret""")
  }

  it should "assume certain caller-save registers are not used by external functions" in {
    val b1 = new AsmBasicBlock(2)
    b1.instructions ++= Seq(
      Mov(ConstOperand(0, 4), RegisterOperand(10, 4)),
      Call(LabelOperand("calloc"), Seq()),
      Mov(RegisterOperand(10, 4), RegisterOperand(RAX, 4))
    )
    val b2 = new AsmBasicBlock(3)
    val main = buildFunction("__main", (b1, b2))

    val prog = new AsmProgram(List(main), new Digraph[AsmFunction](Map(main -> Seq())))

    prog should succeedAllocatingRegistersForProgramWith(Seq(R11), callerSaveRegs = Set(R11), expected =
      """__main:
        |.L0:
        |  movl $0, %r11d
        |  call calloc
        |  movl %r11d, %eax
        |.L3:
        |  ret""")
  }

  it should "not reload operands that can used from memory if they're only used once" in {
    Seq(
      Mov(ConstOperand(0, 4), RegisterOperand(10, 4)),
      Call(LabelOperand("_foobar"), Seq()),
      Mov(ConstOperand(2, 4), RegisterOperand(11, 4)),
      Add(RegisterOperand(10, 4), RegisterOperand(11, 4)),
      Mov(RegisterOperand(11, 4), RegisterOperand(RAX, 4)),
      Ret(4)
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RAX, RCX), callerSaveRegs = Set(RAX, RCX), expected =
      """  movl $0, %eax
        |  movl %eax, 4(%rsp)
        |  call _foobar
        |  movl $2, %eax
        |  addl 4(%rsp), %eax
        |  movl %eax, %eax
        |  ret""")
  }

  it should "always reload operands if they're used more than once" in {
    Seq(
      Mov(ConstOperand(0, 4), RegisterOperand(10, 4)),
      Call(LabelOperand("_foobar"), Seq()),

      Mov(ConstOperand(2, 4), RegisterOperand(11, 4)),
      Add(RegisterOperand(10, 4), RegisterOperand(11, 4)),
      Mov(RegisterOperand(11, 4), AddressOperand(base = Some(RegisterOperand(RSP, 8)), sizeBytes = 4)),

      Mov(ConstOperand(2, 4), RegisterOperand(12, 4)),
      Add(RegisterOperand(10, 4), RegisterOperand(12, 4)),
      Mov(RegisterOperand(12, 4), AddressOperand(base = Some(RegisterOperand(RSP, 8)), sizeBytes = 4))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RAX, RCX), callerSaveRegs = Set(RAX, RCX), expected =
      """  movl $0, %eax
        |  movl %eax, 4(%rsp)
        |  call _foobar
        |  movl $2, %eax
        |  movl 4(%rsp), %ecx
        |  addl %ecx, %eax
        |  movl %eax, (%rsp)
        |  movl $2, %eax
        |  addl %ecx, %eax
        |  movl %eax, (%rsp)""")
  }

  it should "reload registers into a matching physical register" in {
    Seq(
      Mov(ConstOperand(0, 4), RegisterOperand(10, 4)),
      Mov(RegisterOperand(10, 4), AddressOperand(base = Some(RegisterOperand(RSP, 8)), sizeBytes = 4)),
      Call(LabelOperand("_foobar"), Seq()),
      Mov(RegisterOperand(10, 4), RegisterOperand(RDX, 4))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RAX, RDX), callerSaveRegs = Set(RAX, RDX), expected =
      """  movl $0, %eax
        |  movl %eax, (%rsp)
        |  movl %eax, 4(%rsp)
        |  call _foobar
        |  movl 4(%rsp), %edx""")
  }

  it should "treat the WRITE_BEFORE operand specification correctly" in {
    Seq(
      Mov(ConstOperand(0, 4), RegisterOperand(10, 4)),
      new Instruction("dummy", (RegisterOperand(10, 4), READ), (RegisterOperand(RAX, 4), WRITE_BEFORE | IMPLICIT))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RAX, RDX), expected =
      """  movl $0, %edx
        |  dummyl %edx""")
  }

}
