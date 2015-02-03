package mjis

import org.scalatest._
import mjis.asm._
import scala.collection.mutable.ListBuffer

class PeepholeOptimizationTest extends FlatSpec with Matchers with BeforeAndAfter {

  val reg0 = RegisterOperand(0, 4)
  val reg1 = RegisterOperand(1, 4)
  val reg2 = RegisterOperand(2, 4)
  val c1 = ConstOperand(1, 4)

  val mov00 = Mov(reg0, reg0)
  val mov10 = Mov(reg1, reg0)
  val mov01 = Mov(reg0, reg1)
  val mov20 = Mov(reg2, reg0)
  val mov22 = Mov(reg2, reg2)
  val add01 = Add(reg0, reg1)
  val addC10 = Add(c1, reg0)

  it should "remove mov $x $x" in {
    val instructions = ListBuffer(add01, mov22, mov00)
    new PeepholeOptimizer(null).optimizeSequence(instructions) shouldBe ListBuffer(add01)
  }

  it should "replace mov $x $y, mov $z $y with mov $x $y" in {
    val instructions = ListBuffer(mov20, mov10)
    new PeepholeOptimizer(null).optimizeSequence(instructions) shouldBe ListBuffer(mov10)
  }

  it should "replace mov $x $y, mov $y $x with mov $x $y" in {
    val instructions = ListBuffer(mov10, mov01)
    new PeepholeOptimizer(null).optimizeSequence(instructions) shouldBe ListBuffer(mov10)
  }

  it should "apply pattern matching multiple times" in {
    val instructions = ListBuffer(mov10, mov22, mov10)
    new PeepholeOptimizer(null).optimizeSequence(instructions) shouldBe ListBuffer(mov10)
  }

}