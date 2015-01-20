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
      Mov(RegisterOperand(RDI, 4), RegisterOperand(10, 4)),
      Add(ConstOperand(2, 4), RegisterOperand(10, 4)),
      Mov(RegisterOperand(10, 4), RegisterOperand(RAX, 4)),
      Ret(4)
    ) should succeedAllocatingRegistersInstrSeqWith(
      """  movl %edi, %edi
        |  addl $2, %edi
        |  movl %edi, %eax
        |  ret""")
  }

  it should "select a different register if another register is still alive" in {
    Seq(
      Mov(RegisterOperand(RDI, 4), RegisterOperand(10, 4)),
      Mov(RegisterOperand(RCX, 4), RegisterOperand(11, 4)),
      Add(ConstOperand(2, 4), RegisterOperand(11, 4)),
      Mov(RegisterOperand(10, 4), RegisterOperand(RAX, 4)),
      Ret(4)
    ) should succeedAllocatingRegistersInstrSeqWith(
      """  movl %edi, %edi
        |  movl %ecx, %ecx
        |  addl $2, %ecx
        |  movl %edi, %eax
        |  ret""")
  }

  it should "replace registers in AddressOperands" in {
    Seq(
      Mov(RegisterOperand(RDI, 4), RegisterOperand(10, 4)),
      Mov(RegisterOperand(RSI, 4), RegisterOperand(11, 4)),
      Mov(AddressOperand(Some(RegisterOperand(10, 4)), Some(RegisterOperand(11, 4)), 1, 0, 4), RegisterOperand(RDI, 4)),
      Mov(RegisterOperand(RDI, 4), RegisterOperand(RAX, 4)),
      Ret(4)
    ) should succeedAllocatingRegistersInstrSeqWith(
      """  movl %edi, %edi
        |  movl %esi, %esi
        |  movl (%edi,%esi), %edi
        |  movl %edi, %eax
        |  ret""")
  }

  it should "save caller-save registers" in {
    Seq(
      Mov(RegisterOperand(RDI, 4), RegisterOperand(RCX, 4)),
      Mov(RegisterOperand(RSI, 4), RegisterOperand(RDX, 4)),
      Call(LabelOperand("_foobar"), Seq()),
      Add(RegisterOperand(RDX, 4), RegisterOperand(RCX, 4)),
      Mov(RegisterOperand(RCX, 4), RegisterOperand(RAX, 4)),
      Ret(4)
    ) should succeedAllocatingRegistersInstrSeqWith(
      """  subq $8, %rsp
        |  movl %edi, %ecx
        |  movl %esi, %edx
        |  movl %edx, (%rsp)
        |  movl %ecx, 4(%rsp)
        |  call _foobar
        |  movl (%rsp), %edx
        |  movl 4(%rsp), %ecx
        |  addl %edx, %ecx
        |  movl %ecx, %eax
        |  ret
        |  addq $8, %rsp""") // here, ret is not part of the epilogue, that's why addq comes after it
  }

  it should "save callee-save registers" in {
    Seq(
      Mov(RegisterOperand(RDI, 4), RegisterOperand(RBX, 4)),
      Mov(RegisterOperand(RBX, 4), RegisterOperand(RAX, 4)),
      Ret(4)
    ) should succeedAllocatingRegistersInstrSeqWith(
      """  subq $8, %rsp
        |  movq %rbx, (%rsp)
        |  movl %edi, %ebx
        |  movl %ebx, %eax
        |  ret
        |  movq (%rsp), %rbx
        |  addq $8, %rsp""".stripMargin
    )
  }

  it should "handle Phi functions" in {
    val bb1 = new AsmBasicBlock(1)
    val bb2 = new AsmBasicBlock(2)
    val bb3 = new AsmBasicBlock(3)
    val bb4 = new AsmBasicBlock(4)

    // function(i): return (1==1) ? i : 0

    bb1.instructions ++= Seq(
      Mov(RegisterOperand(RDI, 4), RegisterOperand(10, 4)),
      Cmp(ConstOperand(1, 4), ConstOperand(1, 4)),
      JmpConditional(BasicBlockOperand(bb2), Relation.Equal, negate = false),
      Jmp(BasicBlockOperand(bb3))
    )

    bb2.instructions ++= Seq(
      Mov(RegisterOperand(10, 4), RegisterOperand(11, 4)),
      Jmp(BasicBlockOperand(bb4))
    )

    bb3.instructions ++= Seq(
      Mov(ConstOperand(0, 4), RegisterOperand(11, 4)),
      Jmp(BasicBlockOperand(bb4))
    )

    bb4.instructions ++= Seq(
      Mov(RegisterOperand(11, 4), RegisterOperand(RAX, 4)),
      Ret(4)
    )

    val function = new AsmFunction("test")
    function.prologue.successors += bb1
    bb4.successors += function.epilogue
    function.basicBlocks ++= Seq(bb1, bb2, bb3, bb4)

    function should succeedAllocatingRegistersWith(
      """.L0:
        |  movl %edi, %edi
        |  cmp $1, $1
        |  je .L1
        |  jmp .L2
        |.L1:
        |  movl %edi, %edi
        |  jmp .L3
        |.L2:
        |  movl $0, %edi
        |  jmp .L3
        |.L3:
        |  movl %edi, %eax
        |  ret""")
  }

  it should "convert existing ActivationRecordOperands" in {
    Seq(Mov(ActivationRecordOperand(0, 8), RegisterOperand(RAX, 8))) should succeedAllocatingRegistersInstrSeqWith(
    """  movq (%rsp), %rax""")
  }

  it should "spill register contents if necessary" in {
    val instrs = 0.until(13).map(i => Mov(ConstOperand(i, 4), RegisterOperand(10 + i, 4))) ++
     Seq(Mov(ConstOperand(0, 4), RegisterOperand(RAX, 4))) ++
     0.until(13).map(i => Add(RegisterOperand(10 + i, 4), RegisterOperand(RAX, 4))) ++
     Seq(Ret(4))
    instrs should succeedAllocatingRegistersInstrSeqWith(
      """  subq $28, %rsp
        |  movq %rbp, (%rsp)
        |  movq %rbx, 8(%rsp)
        |  movl $0, %ebx
        |  movl $1, %ecx
        |  movl $2, %edx
        |  movl $3, %edi
        |  movl $4, %esi
        |  movl $5, %ebp
        |  movl $6, %r8d
        |  movl $7, %r9d
        |  movl $8, %r10d
        |  movl $9, %r11d
        |  movl $10, 24(%rsp)
        |  movl $11, 20(%rsp)
        |  movl $12, 16(%rsp)
        |  movl $0, %eax
        |  addl %ebx, %eax  # 0
        |  addl %ecx, %eax  # 1
        |  addl %edx, %eax  # 2
        |  addl %edi, %eax  # 3
        |  addl %esi, %eax  # 4
        |  addl %ebp, %eax  # 5
        |  addl %r8d, %eax  # 6
        |  addl %r9d, %eax  # 7
        |  addl %r10d, %eax # 8
        |  addl %r11d, %eax # 9
        |  addl 24(%rsp), %eax # 10
        |  addl 20(%rsp), %eax # 11
        |  addl 16(%rsp), %eax # 12
        |  ret
        |  movq 8(%rsp), %rbx
        |  movq (%rsp), %rbp
        |  addq $28, %rsp""")
  }

}
