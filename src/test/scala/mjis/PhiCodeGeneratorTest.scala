package mjis

import mjis.asm.{Mov, RegisterOperand}
import org.scalatest._

class PhiCodeGeneratorTest extends FlatSpec with Matchers {

  "The Phi code generator" should "circumvent the Swap problem" in {
    val phiMap = Map(
      RegisterOperand(10, 4) -> RegisterOperand(20, 4),
      RegisterOperand(20, 4) -> RegisterOperand(10, 4)
    )

    new PhiCodeGenerator(phiMap).getInstructions() should contain inOrderOnly (
      Mov(RegisterOperand(20, 4), RegisterOperand(0, 4)),
      Mov(RegisterOperand(10, 4), RegisterOperand(20, 4)),
      Mov(RegisterOperand(0, 4), RegisterOperand(10, 4))
    )
  }

  it should "generate code for Phis with multiple outputs" in {
    val phiMap = Map(
      RegisterOperand(50, 4) -> RegisterOperand(30, 4),
      RegisterOperand(40, 4) -> RegisterOperand(30, 4),
      RegisterOperand(30, 4) -> RegisterOperand(10, 4),
      RegisterOperand(10, 4) -> RegisterOperand(20, 4),
      RegisterOperand(20, 4) -> RegisterOperand(10, 4)
    )

    new PhiCodeGenerator(phiMap).getInstructions() should contain inOrderOnly (
      Mov(RegisterOperand(30, 4), RegisterOperand(40, 4)),
      Mov(RegisterOperand(30, 4), RegisterOperand(50, 4)),
      Mov(RegisterOperand(10, 4), RegisterOperand(30, 4)),
      Mov(RegisterOperand(20, 4), RegisterOperand(0, 4)),
      Mov(RegisterOperand(10, 4), RegisterOperand(20, 4)),
      Mov(RegisterOperand(0, 4), RegisterOperand(10, 4))
    )
  }

}
