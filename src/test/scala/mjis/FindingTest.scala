package mjis

import java.io.StringWriter

import org.scalatest._

class FindingTest extends FlatSpec with Matchers {

  "Finding" should "output the correct finding position" in {
    val out = new StringWriter()
    Finding.printAll(List(
      new Finding { val pos = Position(3, 3); val severity = Severity.WARNING; val msg = "A" },
      new Finding { val pos = Position(1, 2); val severity = Severity.ERROR; val msg = "B" }
    ), List("line1\n", "line2\r", "line3\r\n"), out)

    out.toString shouldBe
      """1:2 ERROR: B
        |line1
        | ^
        |3:3 WARNING: A
        |line3
        |  ^
        |""".stripMargin
  }

  it should "output findings without input source" in {
    val out = new StringWriter()
    Finding.printAll(List(
      new Finding { val pos = Position(3, 3); val severity = Severity.WARNING; val msg = "A" },
      new Finding { val pos = Position(1, 2); val severity = Severity.ERROR; val msg = "B" }
    ), Iterable.empty, out)

    out.toString shouldBe
      """1:2 ERROR: B
        |3:3 WARNING: A
        |""".stripMargin
  }
}
