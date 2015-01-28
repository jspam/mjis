package mjis

import mjis.asm._
import org.scalatest._

class InstructionTest extends FlatSpec with Matchers {

  "An instruction" should "define equality over opcode and operands" in {
    Mov(RegisterOperand(1, 4), RegisterOperand(2, 4)) should equal(Mov(RegisterOperand(1, 4), RegisterOperand(2, 4)))
    Mov(RegisterOperand(1, 4), RegisterOperand(2, 4)) shouldNot equal(Mov(RegisterOperand(1, 4), RegisterOperand(3, 4)))
    Mov(RegisterOperand(1, 4), RegisterOperand(2, 4)) shouldNot equal(Sub(RegisterOperand(1, 4), RegisterOperand(2, 4)))
  }

  it should "define equality also over the operand specs" in {
    Ret(4) shouldNot equal(Ret())
  }

  it should "include opcode and operands in its toString representation" in {
    Mov(RegisterOperand(1, 4), RegisterOperand(2, 4)).toString should equal(s"mov ${RegisterOperand(1, 4)}, ${RegisterOperand(2, 4)}")
  }

}
