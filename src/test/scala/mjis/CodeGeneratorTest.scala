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
        |  movl $42, %eax
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for parameters" in {
    fromMembers("public int foo(int x) { return x; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |.L0:
        |  movl %REG0{4}, %eax
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for an arithmetic expression" in {
    fromMembers("public int foo(int x) { return x + 3; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |.L0:
        |  movl %REG0{4}, %REG1{4}
        |  addl $3, %REG1{4}
        |  movl %REG1{4}, %eax
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for a more complex arithmetic expression" in {
    fromMembers("public int foo(int x, int y) { return (x + 3) * (y + 4); }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |  movl %edx, %REG1{4}
        |.L0:
        |  movl %REG0{4}, %REG2{4}
        |  addl $3, %REG2{4}
        |  movl %REG1{4}, %REG3{4}
        |  addl $4, %REG3{4}
        |  movl %REG3{4}, %eax
        |  mull %REG2{4}
        |  movl %eax, %REG4{4}
        |  movl %REG4{4}, %eax
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for array loads" in {
    fromMembers("public int foo(int[] xs, int i) { return xs[i]; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0{8}
        |  movl %edx, %REG1{4}
        |.L0:
        |  movslq %REG1{4}, %REG2{8}
        |  movq %REG2{8}, %REG3{8}
        |  shlq $2, %REG3{8}
        |  movq %REG0{8}, %REG4{8}
        |  addq %REG3{8}, %REG4{8}
        |  movl 0(%REG4{8}), %REG5{4}
        |  movl %REG5{4}, %eax
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for nested array loads" in {
    fromMembers("public int foo(int[] xs, int i) { return xs[xs[i]]; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0{8}   # xs
        |  movl %edx, %REG1{4}   # i
        |.L0:
        |  movslq %REG1{4}, %REG2{8}
        |  movq %REG2{8}, %REG3{8}
        |  shlq $2, %REG3{8}
        |  movq %REG0{8}, %REG4{8}
        |  addq %REG3{8}, %REG4{8}
        |  movl 0(%REG4{8}), %REG5{4}
        |  movslq %REG5{4}, %REG6{8}
        |  movq %REG6{8}, %REG7{8}
        |  shlq $2, %REG7{8}
        |  movq %REG0{8}, %REG8{8}
        |  addq %REG7{8}, %REG8{8}
        |  movl 0(%REG8{8}), %REG9{4}
        |  movl %REG9{4}, %eax
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for array stores" in {
    fromMembers("public void foo(int[] xs, int i, int j) { xs[i] = j; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0{8}
        |  movl %edx, %REG1{4}
        |  movl %ecx, %REG2{4}
        |.L0:
        |  movslq %REG1{4}, %REG3{8}
        |  movq %REG3{8}, %REG4{8}
        |  shlq $2, %REG4{8}
        |  movq %REG0{8}, %REG5{8}
        |  addq %REG4{8}, %REG5{8}
        |  movl %REG2{4}, 0(%REG5{8})
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for member loads" in {
    fromMembers("public int i; public int j; public int foo(Test t) { return t.j; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0{8}
        |.L0:
        |  movq %REG0{8}, %REG1{8}
        |  addq $4, %REG1{8}
        |  movl 0(%REG1{8}), %REG2{4}
        |  movl %REG2{4}, %eax
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for member loads when that member is the result of a function call" in {
    fromMembers("public Test t; public Test foo() { return this.foo().t; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rdi, %REG0{8}
        |.L0:
        |  movq %REG0{8}, %rdi
        |  call _4Test_foo
        |  movq %rax, %REG1{8}
        |  movq 0(%REG1{8}), %REG2{8}
        |  movq %REG2{8}, %rax
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for member stores" in {
    fromMembers("public int i; public int j; public void foo(Test t, int i) { t.j = i; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rsi, %REG0{8}
        |  movl %edx, %REG1{4}
        |.L0:
        |  movq %REG0{8}, %REG2{8}
        |  addq $4, %REG2{8}
        |  movl %REG1{4}, 0(%REG2{8})
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for member stores when that member is the result of a function call" in {
    fromMembers("public Test t; public Test foo() { this.foo().t = null; return this; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movq %rdi, %REG0{8}
        |.L0:
        |  movq %REG0{8}, %rdi
        |  call _4Test_foo
        |  movq %rax, %REG1{8}
        |  movq $0, 0(%REG1{8})
        |  movq %REG0{8}, %rax
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for a method call" in {
    fromMembers("public int foo(int x) { return x; } public int bar() { return foo(42); }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |.L0:
        |  movl %REG0{4}, %eax
        |  jmp .L1
        |.L1:
        |  ret
        |
        |_4Test_bar:
        |  movq %rdi, %REG2{8}
        |.L2:
        |  movq %REG2{8}, %rdi
        |  movl $42, %esi
        |  call _4Test_foo
        |  movl %eax, %REG3{4}
        |  movl %REG3{4}, %eax
        |  jmp .L3
        |.L3:
        |  ret"""))
  }

  it should "generate code for System.out.println" in {
    fromMembers("public void foo() { System.out.println(42); }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |.L0:
        |  movl $42, %edi
        |  call System_out_println
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for calloc" in {
    fromMembers("public Test t; public void foo() { new Test().foo(); }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |.L0:
        |  movl $1, %edi
        |  movl $8, %esi       # rsi doesn't need to be saved
        |  call _calloc
        |  movq %rax, %REG0{8}
        |  movq %REG0{8}, %rdi
        |  call _4Test_foo
        |  jmp .L1
        |.L1:
        |  ret"""))
  }

  it should "generate code for Phis" in {
    fromMembers("public int foo(int argI, boolean argB) { if (argB) argI = 0; return argI; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movb %dl, %REG0{1}  # argB
        |  movl %esi, %REG1{4}  # argI
        |.L0:
        |  cmpb $1, %REG0{1}
        |  je .L1
        |  jmp .L2
        |.L1:
        |  movl $0, %REG2{4}
        |  jmp .L3
        |.L2:
        |  movl %REG1{4}, %REG2{4}
        |  jmp .L3
        |.L3:
        |  movl %REG2{4}, %eax
        |  jmp .L4
        |.L4:
        |  ret"""))
  }

  it should "generate code for comparisons" in {
    fromMembers("public int foo(int argI) { if (argI > 0) return 1; else return 0; }") should succeedGeneratingCodeWith(template(
      """_4Test_foo:
        |  movl %esi, %REG0{4}
        |.L0:
        |  cmpl $0, %REG0{4}
        |  jg .L1
        |  jmp .L2
        |.L1:
        |  movl $1, %eax
        |  jmp .L3
        |.L2:
        |  movl $0, %eax
        |  jmp .L3
        |.L3:
        |  ret"""))
  }

  it should "circumvent the Swap problem when generating code for Phis" in {
    fromMembers("public int foo(int x) { int y = 42; while (x < 5) { int tmp = x; x = y; y = tmp; } return y; }") should
      succeedGeneratingCodeWith(template(
        """_4Test_foo:
          |  movl %esi, %REG0{4}
          |.L0:
          |  movl $42, %REG1{4}         # y => REG1
          |  movl %REG0{4}, %REG2{4}    # x => REG2
          |  jmp .L1
          |.L1:
          |  cmpl $5, %REG2{4}
          |  jl .L2
          |  jmp .L3
          |.L2:
          |  movl %REG1{4}, %REG3{4}    # tmp = y
          |  movl %REG2{4}, %REG1{4}    # y = x
          |  movl %REG3{4}, %REG2{4}    # x = tmp
          |  jmp .L1
          |.L3:
          |  movl %REG1{4}, %eax
          |  jmp .L4
          |.L4:
          |  ret"""))
  }
}
