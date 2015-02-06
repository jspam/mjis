package mjis

import org.scalatest._
import mjis.asm._
import scala.collection.mutable.ListBuffer

class PeepholeOptimizerTest extends FlatSpec with Matchers with BeforeAndAfter {

  val reg0 = RegisterOperand(0, 4)
  val reg1 = RegisterOperand(1, 4)
  val reg2 = RegisterOperand(2, 4)
  val c0 = ConstOperand(0, 4)
  val c1 = ConstOperand(1, 4)

  val mov00 = Mov(reg0, reg0)
  val mov10 = Mov(reg1, reg0)
  val mov01 = Mov(reg0, reg1)
  val mov20 = Mov(reg2, reg0)
  val mov22 = Mov(reg2, reg2)
  val add01 = Add(reg0, reg1)
  val addC10 = Add(c1, reg0)
  val cmp0 = Cmp(reg0, c0)
  val test0 = Test(reg0, reg0)
  val jmpcond = JmpConditional(BasicBlockOperand(new AsmBasicBlock()), firm.Relation.Equal)
  val cmov10 = MovConditional(reg1, reg0, firm.Relation.Equal)

  "Peephole Optimization" should "remove mov $x $x" in {
    val instructions = ListBuffer(add01, mov22, mov00)
    new PeepholeOptimizer(null).optimizeSequence(instructions)
    instructions shouldBe ListBuffer(add01)
  }

  it should "replace mov $x $y, mov $z $y with mov $x $y" in {
    val instructions = ListBuffer(mov20, mov10)
    new PeepholeOptimizer(null).optimizeSequence(instructions)
    instructions shouldBe ListBuffer(mov10)
  }

  it should "replace mov $x $y, mov $y $x with mov $x $y" in {
    val instructions = ListBuffer(mov10, mov01)
    new PeepholeOptimizer(null).optimizeSequence(instructions)
    instructions shouldBe ListBuffer(mov10)
  }

  it should "apply pattern matching multiple times" in {
    val instructions = ListBuffer(mov10, mov22, mov10)
    new PeepholeOptimizer(null).optimizeSequence(instructions)
    instructions shouldBe ListBuffer(mov10)
  }

  it should "replace cmp 0 + jmp with test + jmp" in {
    val instructions = ListBuffer(cmp0, jmpcond)
    new PeepholeOptimizer(null).optimizeSequence(instructions)
    instructions(0) shouldBe test0
    instructions(1) shouldBe jmpcond
    instructions should contain inOrderOnly(test0, jmpcond)
  }

  it should "replace cmp 0 + cmov with test + cmov" in {
    val instructions = ListBuffer(cmp0, cmov10)
    new PeepholeOptimizer(null).optimizeSequence(instructions)
    instructions should contain inOrderOnly(test0, cmov10)
  }

}
