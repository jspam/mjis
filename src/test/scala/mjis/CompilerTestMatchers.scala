package mjis

import org.scalatest.matchers.{MatchResult, Matcher}

trait CompilerTestMatchers {

  class LexerSuccessMatcher() extends Matcher[Lexer] {
    def apply(lexer: Lexer) = {
      lexer.result.length
      MatchResult(
        lexer.success,
        "Lexing failed, expected it to succeed. Findings: " + System.lineSeparator() +
          lexer.findings.mkString(System.lineSeparator()),
        "Lexing succeeded, expected it to fail")
    }
  }

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

  def succeedLexing() = new LexerSuccessMatcher()
  def succeedParsing() = new ParserSuccessMatcher()
}

object CompilerTestMatchers extends CompilerTestMatchers
