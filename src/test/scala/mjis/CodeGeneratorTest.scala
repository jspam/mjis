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

  def template(code: String) =
    s"""  .text
      |  .p2align 4,,15
      |
      |$code
      |
      |__main:
      |.L9000:
      |  jmp .L9001
      |.L9001:
      |  ret
      |""".replace("  ", "\t").stripMargin

  "The code generator" should "generate code for a simple method" in {
    fromMembers("public int foo() { return 42; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |.L0:
        |  movq $42, %rax
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for parameters" in {
    fromMembers("public int foo(int x) { return x; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0
        |.L0:
        |  movq %REG0, %rax
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for an arithmetic expression" in {
    fromMembers("public int foo(int x) { return x + 3; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0
        |.L0:
        |  movq %REG0, %REG1
        |  addq $3, %REG1
        |  movq %REG1, %rax
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for a more complex arithmetic expression" in {
    fromMembers("public int foo(int x, int y) { return (x + 3) * (y + 4); }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0
        |  movq %rdx, %REG1
        |.L0:
        |  movq %REG0, %REG2
        |  addq $3, %REG2
        |  movq %REG1, %REG3
        |  addq $4, %REG3
        |  movq %REG2, %rax
        |  mulq %REG3
        |  movq %rax, %REG4
        |  movq %REG4, %rax
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for array loads" in {
    fromMembers("public int foo(int[] xs, int i) { return xs[i]; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0
        |  movq %rdx, %REG1
        |.L0:
        |  movq %REG1, %REG2
        |  shlq $2, %REG2
        |  movq %REG0, %REG3
        |  addq %REG2, %REG3
        |  movq 0(%REG3), %REG4
        |  movq %REG4, %rax
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for array stores" in {
    fromMembers("public void foo(int[] xs, int i, int j) { xs[i] = j; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0
        |  movq %rdx, %REG1
        |  movq %rcx, %REG2
        |.L0:
        |  movq %REG1, %REG3
        |  shlq $2, %REG3
        |  movq %REG0, %REG4
        |  addq %REG3, %REG4
        |  movq %REG2, 0(%REG4)
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for member loads" in {
    fromMembers("public int i; public int j; public int foo(Test t) { return t.j; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0
        |.L0:
        |  movq %REG0, %REG1
        |  addq $4, %REG1
        |  movq 0(%REG1), %REG2
        |  movq %REG2, %rax
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for member loads when that member is the result of a function call" in {
    fromMembers("public Test t; public Test foo() { return this.foo().t; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rdi, %REG0
        |.L0:
        |  movq %REG0, %rdi
        |  call _4Test_foo
        |  movq %rax, %REG1
        |  movq 0(%REG1), %REG2
        |  movq %REG2, %rax
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for member stores" in {
    fromMembers("public int i; public int j; public void foo(Test t, int i) { t.j = i; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0
        |  movq %rdx, %REG1
        |.L0:
        |  movq %REG0, %REG2
        |  addq $4, %REG2
        |  movq %REG1, 0(%REG2)
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for member stores when that member is the result of a function call" in {
    fromMembers("public Test t; public Test foo() { this.foo().t = null; return this; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rdi, %REG0
        |.L0:
        |  movq %REG0, %rdi
        |  call _4Test_foo
        |  movq %rax, %REG1
        |  movq $0, 0(%REG1)
        |  movq %REG0, %rax
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for a method call" in {
    fromMembers("public int foo(int x) { return x; } public int bar() { return foo(42); }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0
        |.L0:
        |  movq %REG0, %rax
        |  jmp .L1
        |.L1:
        |  ret
        |
        |_4Test_bar:
        |  movq %rdi, %REG2
        |.L2:
        |  movq %REG2, %rdi
        |  movq $42, %rsi
        |  call _4Test_foo
        |  movq %rax, %REG3
        |  movq %REG3, %rax
        |  jmp .L3
        |.L3:
        |  ret"""))
  }

  it should "generate code for System.out.println" in {
    fromMembers("public void foo() { System.out.println(42); }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |.L0:
        |  movq $42, %rdi
        |  call System_out_println
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for calloc" in {
    fromMembers("public void foo() { new Test().foo(); }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |.L0:
        |  movq $1, %rdi
        |  movq $8, %rsi       # rsi doesn't need to be saved
        |  call calloc
        |  movq %rax, %REG0
        |  movq %REG0, %rdi
        |  call _4Test_foo
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for Phis" in {
    fromMembers("public int foo(int argI, boolean argB) { if (argB) argI = 0; return argI; }") should succeedGeneratingCodeWith(template(
    """_4Test_foo:
      |  movq %rdx, %REG0  # argB
      |  movq %rsi, %REG1  # argI
      |.L0:
      |  cmpq $1, %REG0
      |  je .L2
      |  jmp .L1
      |.L1:
      |  movq %REG1, %REG2
      |  jmp .L3
      |.L2:
      |  movq $0, %REG2
      |  jmp .L3
      |.L3:
      |  movq %REG2, %rax
      |  jmp .L4
      |.L4:
      |  ret"""))
  }

  it should "generate code for comparisons" in {
    fromMembers("public int foo(int argI) { if (argI > 0) return 1; else return 0; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
      |  movq %rsi, %REG0
      |.L0:
      |  cmpq $0, %REG0
      |  jg .L1
      |  jmp .L2
      |.L1:
      |  movq $1, %rax
      |  jmp .L3
      |.L2:
      |  movq $0, %rax
      |  jmp .L3
      |.L3:
      |  ret"""))
  }

  it should "circumvent the Swap problem when generating code for Phis" in {
    fromMembers("public int foo(int x) { int y = 42; while (x < 5) { int tmp = x; x = y; y = tmp; } return y; }") should
      succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0
        |.L0:
        |  movq $42, %REG1      # y => REG1
        |  movq %REG0, %REG2    # x => REG2
        |  jmp .L2
        |.L1:
        |  movq %REG1, %REG3    # tmp = y
        |  movq %REG2, %REG1    # y = x
        |  movq %REG3, %REG2    # x = tmp
        |  jmp .L2
        |.L2:
        |  cmpq $5, %REG2
        |  jl .L1
        |  jmp .L3
        |.L3:
        |  movq %REG1, %rax
        |  jmp .L4
        |.L4:
        |  ret"""))
  }
}
