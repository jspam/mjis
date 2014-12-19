package mjis

import firm.Graph
import mjis.util.CCodeGenerator

import org.scalatest.Assertions
import org.scalatest.matchers.{ MatchResult, Matcher, BeMatcher }
import org.scalatest.words.MatcherWords.be
import mjis.ast._
import mjis.opt.Optimization
import mjis.CompilerTestHelper._
import System.{ lineSeparator => n }
import java.io._
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.io.Source
import scala.sys.process._

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
          else Some(s"Expected ${expectedFinding},$n     got ${findings.head}")
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

  class CCodeGeneratorSuccessMatcher(expectedString: String) extends AnalysisPhaseSuccessMatcher[Typer]() {
    override def mkFailureMessage(typer: Typer) = {
      val out = new StringWriter()
      new CCodeGenerator(out).visit(typer.result)
      if (out.toString != expectedString)
        Some(s"Expected:$n'$expectedString'${n}Computed:$n'${out.toString}'${n}Diff:$n'${expectedString diff out.toString}'")
      else None
    }
  }

  class FirmConstructorSuccessMatcher(expectedGraphs: List[Graph]) extends Matcher[String] {
    def apply(input: String) = {
      val expectedPrefix = FirmGraphTestHelper.ExpectedPrefix
      val expectedGraphsMap = (expectedGraphs map { g => {
        assert(g.getEntity.getName.startsWith(expectedPrefix), s"Expected graph names must start with '$expectedPrefix'")
        g.getEntity.getName.drop(expectedPrefix.length) -> g
      }}).toMap

      var success = true
      var failureMessage: String = ""

      val analysisResult = Compiler.exec[Typer](new StringReader(input))
      if (analysisResult.isRight)
        Assertions.fail(s"Analysis failed, expected to succeed. Findings:$n${analysisResult.right.get.mkString(n)}")

      new FirmConstructor(analysisResult.left.get.result).result
      val actualGraphsMap = firm.Program.getGraphs.
        filter(g => !g.getEntity.getName.startsWith(expectedPrefix)).
        map(g => g.getEntity.getName -> g).toMap
      if (actualGraphsMap.size != expectedGraphsMap.size) {
        success = false
        val expGraphs = expectedGraphsMap.keys.mkString(", ")
        val actGraphs = actualGraphsMap.keys.mkString(", ")
        failureMessage = s"Expected ${expectedGraphsMap.size} graph(s): $expGraphs, got ${actualGraphsMap.size}: $actGraphs"
      } else {
        expectedGraphsMap.foreach { case (name, expected) => actualGraphsMap.get(name) match {
          case None =>
            success = false
            failureMessage = s"Missing graph $name"
          case Some(actual) =>
            val error = FirmGraphTestHelper.isIsomorphic(expected, actual)
            if (error.isDefined) {
              success = false
              failureMessage = s"Graphs ${expected.getEntity.getName} and ${actual.getEntity.getName} " +
                s"were not isomorphic: $error"
            }
        }}
      }

      MatchResult(
        success, failureMessage, "Expected FIRM construction to fail, but it succeeded"
      )
    }
  }

  class FirmGraphIsomorphismMatcher(expectedGraph: Graph) extends Matcher[Graph] {
    override def apply(left: Graph): MatchResult = {
      val result = FirmGraphTestHelper.isIsomorphic(left, expectedGraph)
      MatchResult(
        result.isEmpty,
        s"Expected the graphs to be isomorphic, but they weren't:$n${result.getOrElse("")}",
        "Expected the graphs to be not isomorphic, but they were")
    }
  }

  class OptimizerMatcher(under: Optimization, after: Seq[Optimization], before: Seq[Optimization], to: String) extends Matcher[String] {
    override def apply(from: String): MatchResult = {
      assertExec[FirmConstructor](fromMembers(from))
      assertExec[FirmConstructor](fromMembers(to))

      new FirmConstructor(null).dumpResult(null)

      val beforeGraph = firm.Program.getGraphs.find(_.getEntity.getName == "_4Test_before")
      assert(beforeGraph.isDefined)
      val afterGraph = firm.Program.getGraphs.find(_.getEntity.getName == "_4Test_after")
      assert(afterGraph.isDefined)

      after.foreach(_.optimize())

      under.optimize(beforeGraph.get)

      before.foreach(_.optimize())

      new Optimizer(()).dumpResult(null)

      (new FirmGraphIsomorphismMatcher(afterGraph.get))(beforeGraph.get)
    }
  }

  class CodeGeneratorMatcher(expected: String) extends Matcher[String] {
    override def apply(code: String): MatchResult = {
      val codeGenerator = assertExec[CodeGenerator](code)
      // Remove comments (the regex is evaluated line-wise)
      val resultWithoutComments = "\\s*#.*".r.replaceAllIn(codeGenerator.getResult(), "")
      var labels = mutable.HashMap[Int, Int]()
      val resultWithNormalizedLabels = "(?m)^\\.L(\\d+)".r.replaceAllIn(resultWithoutComments, m => {
        val currentLabel = labels.size
        labels += m.group(1).toInt -> currentLabel
        s".L$currentLabel" })
      val resultWithNormalizedJumps = " \\.L(\\d+)".r.replaceAllIn(resultWithNormalizedLabels, m => {
        val newLabel = labels(m.group(1).toInt)
        s" .L$newLabel" })
      BeMatcher(be(expected)).apply(resultWithNormalizedJumps)
    }
  }

  lazy val ClassPath: String = Seq("sbt", "export compile:fullClasspath").lineStream.last

  class IntegrationTestMatcher() extends Matcher[String] {

    def compileCmd(path: String) = Seq("java", "-cp", ClassPath, "mjis/CLIMain", "--compile-firm", path)

    def apply(path: String) = {
      val p = Process(compileCmd(path), None, ("LD_LIBRARY_PATH", "lib")) #&& "./a.out"
      var err = ""
      val out = try {
        p.!!(ProcessLogger(err += _ + "\n"))
      } catch {
        case e: Exception => Assertions.fail(err)
      }

      val check = Source.fromFile(path.stripSuffix("mj") + "check").mkString

      MatchResult(check == out,
        s"""Expected output is not equal to actual output
          |test file: $path
          |expected:
          |$check
          |actual:
          |$out""".stripMargin,
        "Expected output is equal to actual output\n")
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
  def succeedFirmConstructingWith(expectedGraphs: List[Graph]) = new FirmConstructorSuccessMatcher(expectedGraphs)
  def succeedGeneratingCCodeWith(expectedString: String) = new CCodeGeneratorSuccessMatcher(expectedString)
  def succeedGeneratingAssemblerWith(expectedString: String) = new CodeGeneratorMatcher(expectedString)
  def passIntegrationTest() = new IntegrationTestMatcher()
  def beIsomorphicTo(expectedGraph: Graph) = new FirmGraphIsomorphismMatcher(expectedGraph)
  def optimizeTo(under: Optimization, after: Seq[Optimization] = List.empty, before: Seq[Optimization] = List.empty)(to: String) = new OptimizerMatcher(under, after, before, to)
}

object CompilerTestMatchers extends CompilerTestMatchers
