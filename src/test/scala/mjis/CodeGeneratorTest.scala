package mjis

import firm.{Firm, Backend, Mode}
import mjis.CompilerTestHelper._
import mjis.CompilerTestMatchers._
import org.scalatest._

class CodeGeneratorTest extends FlatSpec with Matchers with BeforeAndAfter {

  before {
    Firm.init()

    // tell FIRM we want to output amd64 code
    val modeP = Mode.createReferenceMode(
      "P64", Mode.Arithmetic.TwosComplement, 64, 64)
    Mode.setDefaultModeP(modeP)
    Backend.option("isa=amd64")
  }

  after {
    Firm.finish()
  }

  def template(code: String, lastLabel: Int) =
    s"""  .text
      |  .p2align 4,,15
      |
      |$code
      |
      |__main:
      |.L${lastLabel + 1}:
      |  jmp .L${lastLabel + 2}
      |.L${lastLabel + 2}:
      |  ret
      |""".replace("  ", "\t").stripMargin

  "The code generator" should "generate code for a simple method" in {
    fromMembers("public int foo() { return 42; }") should succeedGeneratingAssemblerWith(template(
      """_4Test_foo:
        |.L0:
        |  movq $42, %rax
        |  jmp .L1
        |.L1:
        |  ret""", 1))
  }

  it should "generate code for parameters" in {
    fromMembers("public int foo(int x) { return x; }") should succeedGeneratingAssemblerWith(template(
      """_4Test_foo:
        |.L0:
        |  movq %rsi, %rax
        |  jmp .L1
        |.L1:
        |  ret""", 1))
  }

  it should "generate code for an arithmetic expression" in {
    fromMembers("public int foo(int x) { return x + 3; }") should succeedGeneratingAssemblerWith(template(
      """_4Test_foo:
        |  subq $8, %rsp
        |.L0:
        |  movq %rsi, %rax
        |  movq $3, %rbx
        |  addq %rbx, %rax
        |  movq %rax, 0(%rsp)
        |  movq 0(%rsp), %rax
        |  jmp .L1
        |.L1:
        |  addq $8, %rsp
        |  ret""", 1))
  }

  it should "generate code for a more complex arithmetic expression" in {
    fromMembers("public int foo(int x, int y) { return (x + 3) * (y + 4); }") should succeedGeneratingAssemblerWith(template(
      """_4Test_foo:
        |  subq $24, %rsp
        |.L0:
        |  movq %rsi, %rax
        |  movq $3, %rbx
        |  addq %rbx, %rax
        |  movq %rax, 16(%rsp)
        |  movq %rdx, %rax
        |  movq $4, %rbx
        |  addq %rbx, %rax
        |  movq %rax, 8(%rsp)
        |  movq 16(%rsp), %rax
        |  movq 8(%rsp), %rbx
        |  mulq %rbx
        |  movq %rax, %rax
        |  movq %rax, 0(%rsp)
        |  movq 0(%rsp), %rax
        |  jmp .L1
        |.L1:
        |  addq $24, %rsp
        |  ret""", 1))
  }

  it should "generate code for a method call" in {
    fromMembers("public int foo(int x) { return x; } public int bar() { return foo(42); }") should succeedGeneratingAssemblerWith(template(
      """_4Test_foo:
        |.L0:
        |  movq %rsi, %rax
        |  jmp .L1
        |.L1:
        |  ret
        |
        |_4Test_bar:
        |  subq $16, %rsp
        |.L2:
        |  movq %rdi, 8(%rsp)
        |  movq %rdi, %rdi
        |  movq $42, %rsi
        |  call _4Test_foo
        |  movq 8(%rsp), %rdi
        |  movq %rax, 0(%rsp)
        |  movq 0(%rsp), %rax
        |  jmp .L3
        |.L3:
        |  addq $16, %rsp
        |  ret""", 3))
  }

  it should "generate code for System.out.println" in {
    fromMembers("public void foo() { System.out.println(42); }") should succeedGeneratingAssemblerWith(template(
      """_4Test_foo:
        |  subq $8, %rsp
        |.L0:
        |  movq %rdi, 0(%rsp)
        |  movq $42, %rdi
        |  call System_out_println
        |  movq 0(%rsp), %rdi
        |  jmp .L1
        |.L1:
        |  addq $8, %rsp
        |  ret""", 1))
  }

  it should "generate code for calloc" in {
    fromMembers("public void foo() { new Test().foo(); }") should succeedGeneratingAssemblerWith(template(
      """_4Test_foo:
        |  subq $16, %rsp
        |.L0:
        |  movq %rdi, 8(%rsp)  # Save rdi (it contains the this parameter)
        |  movq $1, %rdi
        |  movq $8, %rsi       # rsi doesn't need to be saved
        |  call calloc
        |  movq 8(%rsp), %rdi  # Restore rdi
        |  movq %rax, 0(%rsp)
        |  movq %rdi, 8(%rsp)  # Save rdi (again)
        |  movq 0(%rsp), %rdi
        |  call _4Test_foo
        |  movq 8(%rsp), %rdi  # Restore rdi
        |  jmp .L1
        |.L1:
        |  addq $16, %rsp
        |  ret""", 1))
  }

  it should "save registers upon method calls" in {
    fromMembers("public void foo(int arg1, int arg2, int arg3, int arg4, int arg5, int arg6, int arg7) { System.out.println(42); } ") should succeedGeneratingAssemblerWith(template(
      """_4Test_foo:
        |  subq $48, %rsp
        |.L0:
        |  movq %rdi, 40(%rsp)
        |  movq %rsi, 32(%rsp)
        |  movq %rdx, 24(%rsp)
        |  movq %rcx, 16(%rsp)
        |  movq %r8, 8(%rsp)
        |  movq %r9, 0(%rsp)
        |  movq $42, %rdi
        |  call System_out_println
        |  movq 40(%rsp), %rdi
        |  movq 32(%rsp), %rsi
        |  movq 24(%rsp), %rdx
        |  movq 16(%rsp), %rcx
        |  movq 8(%rsp), %r8
        |  movq 0(%rsp), %r9
        |  jmp .L1
        |.L1:
        |  addq $48, %rsp
        |  ret""", 1))
  }
}
