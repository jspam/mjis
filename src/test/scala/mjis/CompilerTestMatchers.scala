package mjis

import org.scalatest.matchers.{ MatchResult, Matcher }
import mjis.ast.SyntaxTree
import System.{ lineSeparator => n }
import mjis.util.PrettyPrinter
import java.io.{StringReader, StringWriter, BufferedWriter}

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
          s"$n  Expected AST: ${expectedAST.get}$n  Computed AST: ${result.get}$n"
        else
          ""
      MatchResult(
        parser.success && (expectedAST.isEmpty || result == expectedAST),
        "Parsing failed, expected it to succeed. " + findError,
        "Parsing succeeded, expected it to fail")
    }
  }

  class PrettyPrinterSuccessMatcher(expectedString: String) extends Matcher[Parser] {
    def apply(parser: Parser) = {
      val result = parser.result
      val stw = new StringWriter();
      new PrettyPrinter(stw).print(result.get)
      val prettyPrint = stw.toString()
      def findError: String = s"$n  Expected String:$n$expectedString$n  Computed String:$n$prettyPrint$n"

      val success = prettyPrint == expectedString
      val error = if (success) "" else findError

      // second pass to make sure round-tripping works and pretty printer is idempotent
      val roundtrip_result = new Parser((new Lexer(new StringReader(prettyPrint))).result).result
      val stw2 = new StringWriter();
      new PrettyPrinter(stw2).print(roundtrip_result.get)
      val prettyPrint2 = stw2.toString()
      def findError2: String = s"$n  Round-tripping failed:$n$prettyPrint2$n  Computed String:$n$prettyPrint$n"

      val success2 = prettyPrint == prettyPrint2
      val error2 = if (success2) "" else findError2

      MatchResult(
        success && success2,
        s"Pretty printing failed, expected it to succeed.$error$error2",
        "Parsing succeeded, expected it to fail")
    }
  }

  def succeedLexing() = new LexerSuccessMatcher()
  def succeedParsing() = new ParserSuccessMatcher(None)
  def succeedParsingWith(expectedAST: SyntaxTree) = new ParserSuccessMatcher(Some(expectedAST))
  def succeedPrettyPrintingWith(expectedString: String) = new PrettyPrinterSuccessMatcher(expectedString)

}

object CompilerTestMatchers extends CompilerTestMatchers
