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
    opt[Unit]("compile-firm") action {(_,x) => x} text ("Compiles the given file using the libFirm backend")
    opt[String]("stop-after-phase") action { (phase, config) =>
      config.copy(stopAfter = phase)
    } text ("Run compiler until specified phase")
    arg[Path]("<file>") optional () action { (file, config) =>
      config.copy(file = Some(file))
    } validate { file =>
      if(!Files.exists(file)) failure(s"File $file does not exist!")
      if(!Files.isReadable(file)) failure(s"File $file is not readable!")
      else success
    } text ("Input files")
    opt[Unit]("from-stdin") optional () action { (phase, config) =>
      config.copy(file = None)
    } text ("Read input from stdin (ignores files)")
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
