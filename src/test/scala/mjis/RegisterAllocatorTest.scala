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

  "The register allocator" should "keep values in one register where possible" in {
    /* function(i) => return i+2 */
    Seq(
      Mov(RegisterOperand(RAX, 4), RegisterOperand(10, 4)),
      Add(ConstOperand(2, 4), RegisterOperand(10, 4)),
      Mov(RegisterOperand(10, 4), RegisterOperand(RDX, 4))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RDX, RAX), Set(),
      """  movl %eax, %eax
        |  addl $2, %eax
        |  movl %eax, %edx""")
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
        offset = Some(RegisterOperand(11, 4)), sizeBytes = 4)),
      Mov(ConstOperand(42, 4), AddressOperand(base = Some(RegisterOperand(10, 8)), sizeBytes = 4))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RDX, RCX), callerSaveRegs = Set(RDX, RCX),
      """  movq $0, %rdx
        |  movq %rdx, (%rsp) # spill
        |  call _foobar
        |  movl $2, %edx
        |  movq (%rsp), %rcx # reload
        |  movl $3, (%rcx,%edx)
        |  movl $42, (%rcx)""")
  }

  it should "handle Phi functions for non-existing predecessors" in {
    val fun = new AsmFunction("test")

    val b1 = new AsmBasicBlock(1)
    fun.prologue.successors += b1
    b1.predecessors += Some(fun.prologue)

    val b2 = new AsmBasicBlock(2)
    b2.predecessors ++= Seq(Some(b1), None)
    b1.successors += b2
    b2.phis += Phi(Seq(ConstOperand(0, 1), ConstOperand(1, 1)), RegisterOperand(10, 1))
    b2.instructions += Mov(RegisterOperand(10, 1), RegisterOperand(RAX, 1))

    fun.basicBlocks = List(fun.prologue, b1, b2, fun.epilogue)

    fun.epilogue.predecessors += Some(b2)
    b2.successors += fun.epilogue
    fun.epilogue.instructions += Ret(1)

    fun should succeedAllocatingRegistersWith(Seq(RAX), Set(),
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
      Mov(ConstOperand(0, 4), AddressOperand(base = Some(RegisterOperand(11, 8)), offset = Some(RegisterOperand(10, 4)), sizeBytes = 4)),
      Mov(RegisterOperand(RSP, 8), RegisterOperand(11, 8))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RAX, RBX), Set(),
      """  movl $0, %eax
        |  movq $0, %rbx
        |  movq $2, %rsp
        |  movl $0, (%rbx,%eax)
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
        |  movl %ecx, (%rsp)  # spill 1
        |  call _foobar
        |  movl (%rsp), %ecx  # reload 1
        |  movl $0, (%ecx)
        |  movl %ecx, (%rsp)  # spill 2
        |  call _foobar
        |  movl (%rsp), %ecx  # reload 2
        |  movl $0, (%ecx)""")
  }

  it should "convert existing ActivationRecordOperands" in {
    Seq(
      Mov(ActivationRecordOperand(0, 8), RegisterOperand(RAX, 8))
    ) should succeedAllocatingRegistersInstrSeqWith(Seq(RAX), Set(),
      """  movq (%rsp), %rax""")
  }

}
