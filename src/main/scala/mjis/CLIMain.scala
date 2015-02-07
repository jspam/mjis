package mjis

import java.nio.file.{Path, Paths, Files}

import scala.collection.immutable.Seq

import scopt.{Read, OptionParser}

object CLIMain extends App {
  
  implicit val pathRead: Read[Path] = Read.reads( Paths.get(_).toAbsolutePath)
  
  val parser = new OptionParser[Config]("java -jar mjc.jar") {
    head("\nmjc", "0.1")

    opt[Unit]("lextest") action { (phase, config) =>
      config.copy(stopAfter = "lexer")
    } text ("Run the lexer and output the result in a standardized format")
    opt[Unit]("print-ast") action { (phase, config) =>
      config.copy(stopAfter = "parser")
    } text ("Run the parser and output the result in a standardized format")
    opt[Unit]("check") action { (phase, config) =>
      config.copy(stopAfter = "semantics")
    } text ("Run all semantics checks and exit")
    opt[Unit]("compile-firm") action { (phase, config) =>
      config.copy(useFirmBackend = true)
    } text ("Compile the given file using the libFirm backend")
    opt[Unit]("generate-c") action { (phase, config) =>
      config.copy(stopAfter = "ccodegen")
    } text ("Convert the input to C and output the result")
    opt[String]('p', "stop-after-phase") action { (phase, config) =>
      config.copy(stopAfter = phase)
    } text ("Run compiler until specified phase")
    opt[Unit]("firm-dump") abbr "fdump" action { (phase, config) =>
      config.copy(firmDump = true)
    } text ("Dump all firm graphs")
    opt[Unit]("timings") action { (phase, config) =>
      config.copy(printTimings = true)
    } text ("Print the running time of each phase to stdout")
    opt[Unit]('v', "verbose") action { (phase, config) =>
      config.copy(verbose = true)
    } text ("Enable debug logging")
    opt[Unit]("no-inline") action { (phase, config) =>
      config.copy(inlining = false)
    } text ("Deactivate inlining")
    arg[Path]("<file>") optional () action { (file, config) =>
      config.copy(file = Some(file))
    } validate { file =>
      if(!Files.exists(file)) failure(s"File $file does not exist!")
      else if(!Files.isReadable(file)) failure(s"File $file is not readable!")
      else success
    } text ("Input files")
    opt[Unit]("from-stdin") optional () action { (phase, config) =>
      config.copy(file = None)
    } text ("Read input from stdin (ignores files)")
    opt[Path]('o', "out-file") optional () action { (file, config) =>
      config.copy(outFile = file)
    } text ("File path of output")
  }

  parser.parse(args, Config()) map { config =>
    if (config.file == null) {
      parser.reportError("Must specify at least one file or --from-stdin")
      parser.showUsage
      System.exit(1)
    } else {
      System.exit(if (Compiler.compile(config)) 0 else 2)
    }
  } getOrElse {
    System.exit(1)
  }
}
