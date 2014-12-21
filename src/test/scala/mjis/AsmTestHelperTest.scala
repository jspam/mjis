package mjis

import org.scalatest._
import mjis.CompilerTestMatchers._

class AsmTestHelperTest extends FlatSpec with Matchers {

  "The Assembler test helper" should "detect simple isomorphic assembler code" in {
    val left =
      """
        |  .text
        |  .p2align 4,,15
        |
        |_4Test_foo:
        |  subq $8, %rsp
        |.L88 # Block 88
        |  jmp .L86
        |.L86:
        |  addq $8, %rsp
        |  ret
      """.replace("  ", "\t").stripMargin
    val right =
      """
        |  .text
        |  .p2align 4,,15
        |
        |_4Test_foo:
        |  subq $8, %rsp
        |.L0
        |  jmp .L1
        |.L1:
        |  addq $8, %rsp
        |  ret
      """.replace("  ", "\t").stripMargin

    left should beIsomorphicAsmTo (right)
  }

  it should "detect isomorphic assembler code using labels with fall-throughs" in {
    val left =
      """
        |.L88:
        |  movq %rax, %rbx
        |.L86:
        |  addq $8, %rsp
        |  jmp .L88
      """.replace("  ", "\t").stripMargin
    val right =
      """
        |.L0:
        |  movq %rax, %rbx
        |.L1:
        |  addq $8, %rsp
        |  jmp .L0
      """.replace("  ", "\t").stripMargin

    left should beIsomorphicAsmTo(right)
  }

  it should "detect non-isomorphic assembler code" in {
    val left =
      """
        |  movq %rax, %rbx
      """.replace("  ", "\t").stripMargin
    val right =
      """
        |  movq %rbx, %rbx
      """.replace("  ", "\t").stripMargin

    left shouldNot beIsomorphicAsmTo(right)
  }

  it should "detect non-isomorphic assembler code with labels" in {
    val left =
      """
        |.L88:
        |  jmp .L89
        |.L89
        |  ret
      """.replace("  ", "\t").stripMargin
    val right =
      """
        |.L42:
        |  jmp .L42
        |.L23:
        |  ret
      """.replace("  ", "\t").stripMargin

    left shouldNot beIsomorphicAsmTo(right)
  }

  it should "detect isomorphic assembler code when blocks are swapped" in {
    val left =
      """
        |.L55:
        |  jne .L76
        |  jmp .L78
        |.L78:
        |  jmp .L75
        |.L75:
        |  jmp .L53
        |.L76:
        |  jmp .L75
        |.L53:
        |  ret
      """.replace("  ", "\t").stripMargin
    val right =
      """
        |.L0:
        |  jne .L1
        |  jmp .L2
        |.L1:
        |  jmp .L3
        |.L2:
        |  jmp .L3
        |.L3:
        |  jmp .L4
        |.L4:
        |  ret
      """.replace("  ", "\t").stripMargin

    left should beIsomorphicAsmTo(right)
  }
}
