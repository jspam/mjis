package mjis

import org.scalatest._

class PositionTest extends FlatSpec with Matchers with Inspectors {

  "The Position class" should "pretty print the position in the current line" in {
    val position = new Position(0, 10, "123\t567890")
    position.longString should equal("123\t567890" + System.lineSeparator() + "   \t     ^")
  }

}
