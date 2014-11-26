package mjis

import firm.Graph
import org.scalatest.matchers.{ MatchResult, Matcher }
import mjis.ast._
import System.{ lineSeparator => n }
import java.io.{StringReader, StringWriter, BufferedWriter}

import scala.reflect.ClassTag

trait CompilerTestMatchers {

  class AnalysisPhaseSuccessMatcher[P <: AnalysisPhase[_]: ClassTag]() extends Matcher[String] {
    def mkFailureMessage(phase: P): Option[String] = None
    def apply(input: String) = {
      val result = Compiler.exec[P](new StringReader(input))
      val failureMessage = result match {
        case Left(phase) => mkFailureMessage(phase)
        case _ => Some(s"Failed, expected to succeed. Findings:$n${result.right.get.mkString(n)}")
      }

      MatchResult(failureMessage.isEmpty, failureMessage.getOrElse(""), "Succeeded, expected to fail")
    }
  }

  class AnalysisPhaseSuccessWithMatcher[O, P <: AnalysisPhase[O]: ClassTag](expected: O) extends AnalysisPhaseSuccessMatcher[P] {
    override def mkFailureMessage(phase: P): Option[String] =
      if (expected == phase.result) None
      else Some(s"Expected: $expected${n}Computed: ${phase.result}")
  }

  class AnalysisPhaseFailureWithMatcher[P <: AnalysisPhase[_]: ClassTag](expectedFinding: Finding) extends Matcher[String] {
    def apply(input: String) = {
      val result = Compiler.exec[P](new StringReader(input))
      val failureMessage = result match {
        case Right(findings) =>
          if (expectedFinding == findings.head) None
          else Some(s"Expected ${expectedFinding}, got ${findings.head}")
        case _ => Some(s"Succeeded, expected to fail.")
      }

      MatchResult(failureMessage.isEmpty, failureMessage.getOrElse(""), "Why would you negate this??")
    }
  }

  class PrettyPrinterSuccessMatcher(expectedString: String) extends AnalysisPhaseSuccessMatcher[Parser]() {
    override def mkFailureMessage(parser: Parser) = {
      val out = new StringWriter()
      parser.dumpResult(new BufferedWriter(out))
      if (out.toString != expectedString)
        Some(s"Expected:$n'$expectedString'${n}Computed:$n'${out.toString}'")
      else {
        // second pass to make sure round-tripping works and pretty printer is idempotent
        Compiler.exec[Parser](new StringReader(out.toString)) match {
          case Left(parser2) =>
            val out2 = new StringWriter()
            parser2.dumpResult(new BufferedWriter(out2))
            if (out2.toString == expectedString) None
            else Some(s"Round-tripping failed:$n${out2.toString}${n}Original String:$n${out.toString}$n")
          case Right(findings) => Some(s"Round-tripping failed:$n$findings")
        }
      }
    }
  }

  class FirmGraphIsomorphismMatcher(expectedGraph: Graph) extends Matcher[Graph] {
    override def apply(left: Graph): MatchResult = {
      val result = FirmGraphTestHelper.isIsomorphic(left, expectedGraph)
      MatchResult(
        result.isEmpty,
        s"Expected the graphs to be isomorphic, but they weren't: $result",
        "Expected the graphs to be not isomorphic, but they were")
    }
  }

  def succeedLexing() = new AnalysisPhaseSuccessMatcher[Lexer]()
  def succeedParsing() = new AnalysisPhaseSuccessMatcher[Parser]()
  def succeedParsingWith(expectedAST: Program) = new AnalysisPhaseSuccessWithMatcher[Program, Parser](expectedAST)
  def failParsingWith(expectedFinding: Finding) = new AnalysisPhaseFailureWithMatcher[Parser](expectedFinding)
  def succeedPrettyPrintingWith(expectedString: String) = new PrettyPrinterSuccessMatcher(expectedString)
  def succeedTyping = new AnalysisPhaseSuccessMatcher[Typer]()
  def failTypingWith(expectedFinding: Finding) = new AnalysisPhaseFailureWithMatcher[Typer](expectedFinding)
  def succeedNaming() = new AnalysisPhaseSuccessMatcher[Namer]()
  def failNamingWith(expectedFinding: Finding) = new AnalysisPhaseFailureWithMatcher[Namer](expectedFinding)
  def beIsomorphicTo(expectedGraph: Graph) = new FirmGraphIsomorphismMatcher(expectedGraph)
}

object CompilerTestMatchers extends CompilerTestMatchers
