package mjis

import org.scalatest._

class PositionTest extends FlatSpec with Matchers with Inspectors {

  "A Position" should "pretty print the position in the current line" in {
    val position = new Position(0, 6, "123\t567\n")
    position.longString should equal("123\t567" + System.lineSeparator() + "   \t ^")
  }

  it should "pretty print even when the current line does not end with a line break" in {
    val position = new Position(0, 10, "123\t567890")
    position.longString should equal("123\t567890" + System.lineSeparator() + "   \t     ^")
  }

}
