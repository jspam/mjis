package mjis

import org.scalatest.matchers.{MatchResult, Matcher}

trait ParserTestMatchers {

  class ParserSuccessMatcher() extends Matcher[Parser] {

    def apply(parser: Parser) = {
      parser.result
      MatchResult(
        parser.success,
        "Parsing failed, expected it to succeed. Findings: " + System.lineSeparator() +
          parser.findings.mkString(System.lineSeparator()),
        "Parsing succeeded, expected it to fail")
    }
  }

  def succeedParsing() = new ParserSuccessMatcher()
}

object ParserTestMatchers extends ParserTestMatchers