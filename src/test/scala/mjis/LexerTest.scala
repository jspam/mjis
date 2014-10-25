package mjis

import org.scalatest._

class LexerTest extends FlatSpec with Matchers {

  "The lexer" should "return an empty token list when parsing the empty string" in {
    val lexResult = new Lexer().process("")
    lexResult.success shouldBe true
    lexResult.result shouldBe empty
    lexResult.findings shouldBe empty
  }

}
