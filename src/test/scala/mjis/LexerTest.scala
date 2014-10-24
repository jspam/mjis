package mjis

import org.scalatest._

class LexerTest extends FlatSpec with Matchers {

  "The lexer" should "return an empty token list when parsing the empty string" in {
    new Lexer().process("") shouldBe empty
  }

}
