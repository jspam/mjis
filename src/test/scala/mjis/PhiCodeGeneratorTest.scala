package mjis

import mjis.asm._
import org.scalatest._

class PhiCodeGeneratorTest extends FlatSpec with Matchers {

  "The Phi code generator" should "circumvent the Swap problem" in {
    val phiMap: Map[Operand, Operand] = Map(
      RegisterOperand(10, 4) -> RegisterOperand(20, 4),
      RegisterOperand(20, 4) -> RegisterOperand(10, 4)
    )

    new PhiCodeGenerator(phiMap, 100, 101).getInstructions() should contain inOrderOnly (
      Mov(RegisterOperand(10, 4), RegisterOperand(100, 4)),
      Mov(RegisterOperand(20, 4), RegisterOperand(10, 4)),
      Mov(RegisterOperand(100, 4), RegisterOperand(20, 4))
    )
  }

  it should "generate instructions for a longer cycle" in {
    val phiMap: Map[Operand, Operand] = Map(
      RegisterOperand(10, 4) -> RegisterOperand(20, 4),
      RegisterOperand(20, 4) -> RegisterOperand(30, 4),
      RegisterOperand(30, 4) -> RegisterOperand(40, 4),
      RegisterOperand(40, 4) -> RegisterOperand(10, 4)
    )

    new PhiCodeGenerator(phiMap, 100, 101).getInstructions() should contain inOrderOnly (
      Mov(RegisterOperand(30, 4), RegisterOperand(100, 4)),
      Mov(RegisterOperand(40, 4), RegisterOperand(30, 4)),
      Mov(RegisterOperand(10, 4), RegisterOperand(40, 4)),
      Mov(RegisterOperand(20, 4), RegisterOperand(10, 4)),
      Mov(RegisterOperand(100, 4), RegisterOperand(20, 4))
    )
  }

  it should "generate code for Phis with multiple outputs" in {
    val phiMap: Map[Operand, Operand] = Map(
      RegisterOperand(50, 4) -> RegisterOperand(30, 4),
      RegisterOperand(40, 4) -> RegisterOperand(30, 4),
      RegisterOperand(30, 4) -> RegisterOperand(10, 4),
      RegisterOperand(10, 4) -> RegisterOperand(20, 4),
      RegisterOperand(20, 4) -> RegisterOperand(10, 4)
    )

    new PhiCodeGenerator(phiMap, 100, 101).getInstructions() should contain inOrderOnly (
      Mov(RegisterOperand(30, 4), RegisterOperand(40, 4)),
      Mov(RegisterOperand(30, 4), RegisterOperand(50, 4)),
      Mov(RegisterOperand(10, 4), RegisterOperand(30, 4)),
      Mov(RegisterOperand(10, 4), RegisterOperand(100, 4)),
      Mov(RegisterOperand(20, 4), RegisterOperand(10, 4)),
      Mov(RegisterOperand(100, 4), RegisterOperand(20, 4))
    )
  }

  it should "generate moves for ActivationRecordOperands" in {
    val phiMap: Map[Operand, Operand] = Map(
      ActivationRecordOperand(0, 4) -> RegisterOperand(20, 4),
      ActivationRecordOperand(1, 4) -> ActivationRecordOperand(0, 4)
    )

    new PhiCodeGenerator(phiMap, 100, 101).getInstructions() should contain inOrderOnly (
      Mov(ActivationRecordOperand(0, 4), RegisterOperand(101, 4)),
      Mov(RegisterOperand(101, 4), ActivationRecordOperand(1, 4)),
      Mov(RegisterOperand(20, 4), ActivationRecordOperand(0, 4))
    )
  }

  it should "order operands of different size" in {
    val phiMap: Map[Operand, Operand] = Map(
      RegisterOperand(10, 8) -> RegisterOperand(20, 8),
      RegisterOperand(20, 4) -> RegisterOperand(10, 4)
    )

    new PhiCodeGenerator(phiMap, 100, 101).getInstructions() should contain inOrderOnly (
      Mov(RegisterOperand(10, 4), RegisterOperand(100, 4)),
      Mov(RegisterOperand(20, 8), RegisterOperand(10, 8)),
      Mov(RegisterOperand(100, 4), RegisterOperand(20, 4))
    )
  }

}
