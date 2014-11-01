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
    opt[String]("stop-after-phase") action { (phase, config) =>
      config.copy(stopAfter = phase)
    } text ("Run compiler until specified phase")
    arg[Path]("<file>...") unbounded () action { (file, config) =>
      config.copy(files = config.files :+ file)
    } validate { file =>
      if(!Files.exists(file)) failure(s"File $file does not exist!")
      else success
    } text ("Input files")
  }

  parser.parse(args, Config()) map { config =>
    System.exit(if (Compiler.compile(config)) 0 else 2)
  } getOrElse {
    System.exit(1)
  }
}

case class Config(stopAfter: String = "", files: Seq[Path] = Seq())
