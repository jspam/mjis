package mjis

import firm.{Relation, Firm}
import mjis.asm._
import mjis.asm.AMD64Registers._
import org.scalatest._
import mjis.CompilerTestMatchers._

class RegisterAllocatorTest extends FlatSpec with Matchers with BeforeAndAfter {

  before {
    Firm.init()
  }

  after {
    Firm.finish()
  }

  def buildFunction(cfEdges: (AsmBasicBlock, AsmBasicBlock)*) = {
    val fun = new AsmFunction("test")

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
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RDX, RAX), Set(),
      """  # movl %eax, %eax (optimized away)
        |  addl $2, %eax
        |  movl %eax, %edx""")
  }

  it should "avoid blocked registers" in {
    Seq(
      Mov(ConstOperand(0, 4), RegisterOperand(10, 4)),
      Call(LabelOperand("_foobar"), Seq()), // blocks EAX
      Mov(RegisterOperand(10, 4), AddressOperand(base = Some(RegisterOperand(RSP, 8)), sizeBytes = 4))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RAX, RBX), callerSaveRegs = Set(RAX),
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
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RAX) /* we only have one register */, Set(),
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

  it should "respect liveness intervals ending at the following instruction when reloading" in {
    Seq(
      Mov(ConstOperand(0, 8), RegisterOperand(10, 8)), // gets assigned EDX
      Call(LabelOperand("_foobar"), Seq()),            // blocks both registers => EDX gets spilled
      Mov(ConstOperand(2, 4), RegisterOperand(11, 4)), // gets assigned EDX (reg10 is spilled!)
      // REG10 is reloaded before this instruction, but may not use EDX although its liveness interval ends here.
      Mov(ConstOperand(3, 4), AddressOperand(base = Some(RegisterOperand(10, 8)),
        indexAndScale = Some((RegisterOperand(11, 4), 1)), sizeBytes = 4)),
      Mov(ConstOperand(42, 4), AddressOperand(base = Some(RegisterOperand(10, 8)), sizeBytes = 4))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RDX, RCX), callerSaveRegs = Set(RDX, RCX),
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

    fun should succeedAllocatingRegistersWith(Seq(RAX), Set(),
      """.L1:
        |.L2:
        |  movb $0, %al  # from resolving phi
        |  # movb %al, %al # optimized away # reg 10 has been assigned register al
        |  ret""")
  }

  it should "leave rsp alone" in {
    Seq(
      Mov(ConstOperand(0, 4), RegisterOperand(10, 4)),
      Mov(ConstOperand(0, 8), RegisterOperand(11, 8)),
      Mov(ConstOperand(2, 4), RegisterOperand(RSP, 8)),
      Mov(ConstOperand(0, 4), AddressOperand(base = Some(RegisterOperand(11, 8)), indexAndScale = Some((RegisterOperand(10, 4), 1)), sizeBytes = 4)),
      Mov(RegisterOperand(RSP, 8), RegisterOperand(11, 8))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RAX, RBX), Set(),
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
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RCX), callerSaveRegs = Set(RCX),
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
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RAX), Set(),
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

    fun should succeedAllocatingRegistersWith(Seq(RAX), Set(),
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
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RAX, RBX), callerSaveRegs = Set(RAX),
      """  movl $0, %ebx
        |  call _foobar
        |  # movl %ebx, %ebx # optimized away
        |  movl %ebx, (%rsp)""")
  }

  it should "automatically select a temporary register for phi generation" in {
    val b0 = new AsmBasicBlock(0)
    val b1 = new AsmBasicBlock(1)
    b1.phis += Phi(Seq(ConstOperand(2, 4), RegisterOperand(10, 4)), RegisterOperand(11, 4))
    b1.phis += Phi(Seq(ConstOperand(1, 4), RegisterOperand(11, 4)), RegisterOperand(10, 4))
    val b2 = new AsmBasicBlock(2)
    val b3 = new AsmBasicBlock(3)

    buildFunction((b0, b1), (b1, b2), (b2, b1), (b1, b3)) should succeedAllocatingRegistersWith(Seq(RAX, RCX, RDX), Set(),
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

    buildFunction((b0, b1), (b1, b2), (b2, b1), (b1, b3)) should succeedAllocatingRegistersWith(Seq(RAX, RCX, RDX), Set(),
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

    buildFunction((b0, b1), (b1, b2), (b2, b1), (b1, b3)) should succeedAllocatingRegistersWith(Seq(RAX, RCX), Set(),
      """  subq $16, %rsp
        |.L0:
        |  movl $2, %eax
        |  movl $1, %ecx
        |.L1:
        |.L2:
        |  movl %ecx, (%rsp)  # spill
        |  movl %eax, 8(%rsp) # spill
        |  movl 8(%rsp), %eax
        |  movl (%rsp), %ecx
        |  movl %ecx, 8(%rsp)
        |  movl %eax, (%rsp)
        |  movl (%rsp), %ecx  # reload
        |  movl 8(%rsp), %eax # reload
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

    fun should succeedAllocatingRegistersWith(Seq(RAX), callerSaveRegs = Set(RAX),
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

    fun should succeedAllocatingRegistersWith(Seq(RAX), callerSaveRegs = Set(RAX),
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

}
