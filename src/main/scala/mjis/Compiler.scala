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

object Compiler {
  private val pipeline: List[Class[_ <: Phase[_]]] = List(classOf[Lexer], classOf[Parser], classOf[Namer], classOf[Typer],
    classOf[FirmConstructor], classOf[CodeGenerator])

  private val stopAfterTargets = Map[String, Class[_ <: Phase[_]]](
    "lexer" -> classOf[Lexer],
    "parser" -> classOf[Parser],
    "semantics" -> classOf[Typer],
    "firm" -> classOf[FirmConstructor],
    "codegen" -> classOf[CodeGenerator]
  )

  def exec(input: Reader, until: Class[_ <: Phase[_]]): Either[Phase[_], List[Finding]] = {
    val phases = ListBuffer[Phase[AnyRef]](new Lexer(input))
    for (cls <- pipeline.tail) {
      val result = phases.last.result

      val findings = phases.flatMap(_.findings)
      if (findings.exists(_.severity == Severity.ERROR))
        return Right(findings.toList)

      phases += cls.getConstructors.asInstanceOf[Array[Constructor[_ <: AnyRef]]].head.newInstance(phases.last.result).asInstanceOf[Phase[AnyRef]]

      if (cls == until) {
        phases.last.forceResult()

        val findings = phases.flatMap(_.findings)
        if (findings.exists(_.severity == Severity.ERROR))
          return Right(findings.toList)

        return Left(phases.last)
      }
    }
    throw new IllegalArgumentException(s"Unknown phase: $until")
  }

  def exec[P <: Phase[_]: ClassTag](input: Reader): Either[P, List[Finding]] =
    exec(input, classTag[P].runtimeClass.asInstanceOf[Class[_ <: Phase[_]]]).asInstanceOf[Either[P, List[Finding]]]

  def compile(config: Config): Boolean = {
    Firm.init()

    val fileOrStdIn = config.file.map(f => new FileInputStream(f.toFile)).getOrElse(System.in)
    val input = new InputStreamReader(new BufferedInputStream(fileOrStdIn), "ASCII")
    val target = if (config.stopAfter != "") stopAfterTargets(config.stopAfter) else pipeline.last
    // tell FIRM we want to output amd64 code
    val modeP = Mode.createReferenceMode(
      "P64", Mode.Arithmetic.TwosComplement, 64, 64)
    Mode.setDefaultModeP(modeP)
    Backend.option("isa=amd64")

    exec(input, target) match {
      case Left(phase) =>
        if (config.stopAfter != "") {
          val out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(java.io.FileDescriptor.out), "ASCII"))
          phase.dumpResult(out)
          out.close()
        }
        true
      case Right(findings) =>
        val inputLines = config.file match {
          case Some(file) => Files.lines(file).iterator().toIterable
          case None       => Iterable.empty
        }
        val out = new OutputStreamWriter(System.err)
        Finding.printAll(findings, inputLines, out)
        out.close()
        if (config.stopAfter != "")
          System.out.println("error")
        false
    }
  }
}
