package mjis

import org.scalatest.matchers.{ MatchResult, Matcher }
import mjis.ast.SyntaxTree
import System.{lineSeparator => n}

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

  class ParserSuccessMatcher(expectedAST: Option[SyntaxTree]) extends Matcher[Parser] {
    def apply(parser: Parser) = {
      val result = parser.result
      def findError: String =
        if (!parser.success) s"Findings:$n${parser.findings.mkString(n)}"
        else if (expectedAST.isDefined)
          s"$n  Expected AST: ${expectedAST.get}$n  Computed AST: $result$n"
        else
          ""
      MatchResult(
        parser.success && (expectedAST.isEmpty || result == expectedAST),
        "Parsing failed, expected it to succeed. " + findError,
        "Parsing succeeded, expected it to fail")
    }
  }

  def succeedLexing() = new LexerSuccessMatcher()
  def succeedParsing() = new ParserSuccessMatcher(None)
  def succeedParsingWith(expectedAST: SyntaxTree) = new ParserSuccessMatcher(Some(expectedAST))
}

object CompilerTestMatchers extends CompilerTestMatchers
