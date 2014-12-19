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
        |  movq 16(%rsp), %rax
        |  jmp .L1
        |.L1:
        |  ret""", 1))
  }

  it should "generate code for an arithmetic expression" in {
    fromMembers("public int foo(int x) { return x + 3; }") should succeedGeneratingAssemblerWith(template(
      """_4Test_foo:
        |  subq $8, %rsp
        |.L0:
        |  movq 24(%rsp), %rax
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
        |  movq 40(%rsp), %rax
        |  movq $3, %rbx
        |  addq %rbx, %rax
        |  movq %rax, 16(%rsp)
        |  movq 48(%rsp), %rax
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
        |  movq 16(%rsp), %rax
        |  jmp .L1
        |.L1:
        |  ret
        |
        |_4Test_bar:
        |  subq $8, %rsp
        |.L2:
        |  movq $42, %rax
        |  pushq %rax
        |  movq 24(%rsp), %rax
        |  pushq %rax
        |  call _4Test_foo
        |  addq $16, %rsp
        |  movq %rax, 0(%rsp)
        |  movq 0(%rsp), %rax
        |  jmp .L3
        |.L3:
        |  addq $8, %rsp
        |  ret""", 3))
  }

}
