package mjis

import java.lang.reflect.Constructor
import java.io._
import java.nio.file.Files
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import scala.reflect._
import firm.Firm
import firm.Mode
import firm.Backend
import mjis.util.CCodeGenerator

object Compiler {
  type Pipeline = List[Class[_ <: Phase[_]]]
  private val defaultPipeline: Pipeline = List(classOf[Lexer], classOf[Parser], classOf[Namer],
    classOf[Typer], classOf[FirmConstructor], classOf[Optimizer], classOf[CodeGenerator])
  private val firmCompilePipeline: Pipeline = List(classOf[Lexer], classOf[Parser], classOf[Namer],
    classOf[Typer], classOf[FirmConstructor], classOf[Optimizer], classOf[FirmCodeGenerator])

  private val stopAfterTargets = Map[String, Class[_ <: Phase[_]]](
    "lexer" -> classOf[Lexer],
    "parser" -> classOf[Parser],
    "semantics" -> classOf[Typer],
    "ccodegen" -> classOf[Typer],
    "firm" -> classOf[FirmConstructor],
    "optimizer" -> classOf[Optimizer],
    "codegen" -> classOf[CodeGenerator]
  )

  /**
   * Executes the compiler pipeline up to and including the specified Phase and returns
   * the phase on success, or a list of findings on failure.
   * Note that for until == classOf[Lexer], this method will always return the phase
   * and you'll have to check for success yourself. This is because the lexer's result
   * is only iterable once.
   */
  def exec(input: Reader, until: Class[_ <: Phase[_]], pipeline: Pipeline = defaultPipeline): Either[Phase[_], List[Finding]] = {
    val phases = ListBuffer[Phase[AnyRef]]()
    for (cls <- pipeline) {
      if (phases.isEmpty)
        // need to special case the lexer because it has multiple constructors,
        // so .head won't necessarily pick out the right one
        phases += new Lexer(input)
      else {
        assert(cls.getConstructors.size == 1)
        phases += cls.getConstructors.head.asInstanceOf[Constructor[_ <: AnyRef]].
          newInstance(phases.last.result).asInstanceOf[Phase[AnyRef]]
      }

      // Don't force the Lexer result because it is iterable only once.
      if (cls != classOf[Lexer])
        phases.last.forceResult()

      val findings = phases.flatMap(_.findings)
      if (findings.exists(_.severity == Severity.ERROR))
        return Right(findings.toList)

      if (cls == until)
        return Left(phases.last)
    }
    throw new IllegalArgumentException(s"Unknown phase: $until")
  }

  def exec[P <: Phase[_]: ClassTag](input: Reader): Either[P, List[Finding]] =
    exec(input, classTag[P].runtimeClass.asInstanceOf[Class[_ <: Phase[_]]]).asInstanceOf[Either[P, List[Finding]]]

  def compile(config: Config): Boolean = {
    val pipeline = if (config.useFirmBackend) firmCompilePipeline else defaultPipeline
    Firm.init()

    val fileOrStdIn = config.file.map(f => new FileInputStream(f.toFile)).getOrElse(System.in)
    val input = new InputStreamReader(new BufferedInputStream(fileOrStdIn), "ASCII")
    val target = if (config.stopAfter != "") stopAfterTargets(config.stopAfter) else pipeline.last
    // tell FIRM we want to output amd64 code
    val modeP = Mode.createReferenceMode(
      "P64", Mode.Arithmetic.TwosComplement, 64, 64)
    Mode.setDefaultModeP(modeP)
    Backend.option("isa=amd64")

    def printFindings(findings: Seq[Finding], printErrorText: Boolean) = {
      val inputLines = config.file match {
        case Some(file) => Files.lines(file).iterator().toIterable
        case None       => Iterable.empty
      }
      val out = new OutputStreamWriter(System.err)
      Finding.printAll(findings, inputLines, out)
      out.close()
      if (printErrorText)
        System.out.println("error")
    }

    exec(input, target, pipeline) match {
      case Left(phase) =>
        if (config.stopAfter != "") {
          val out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(java.io.FileDescriptor.out), "ASCII"))
          if (config.stopAfter == "ccodegen")
            phase.asInstanceOf[Typer].result.accept(new CCodeGenerator(out))
          else
            phase.dumpResult(out)
          out.close()
        }
        if (config.stopAfter == "lexer") {
          // exec does not check for success of the lexer
          val success = phase.asInstanceOf[Lexer].success
          printFindings(phase.findings, printErrorText = !success)
          success
        } else {
          true
        }
      case Right(findings) =>
        printFindings(findings, printErrorText = config.stopAfter != "")
        false
    }
  }
}
