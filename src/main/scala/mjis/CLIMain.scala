package mjis

import java.io.File
import java.nio.file.{Path, Paths, Files, FileSystems}

import scala.collection.immutable.Seq

import scopt.{Read, OptionParser}

object CLIMain extends App {
  
  implicit val pathRead: Read[Path] = Read.reads( Paths.get(_).toAbsolutePath)
  
  val parser = new OptionParser[Config]("java -jar mjc.jar") {
    head("\nmjc", "0.1")

    opt[String]("lextest") action { (phase, config) =>
      config.copy(stopAfter = "lexer")
    }
    opt[String]("stop-after-phase") action { (phase, config) =>
      config.copy(stopAfter = phase)
    } text ("Run compiler until specified phase")
    arg[Path]("<file>...") unbounded () action { (file, config) =>
      config.copy(files = config.files :+ file)
    } validate { file =>
      val fileName = file.toString
      if (!fileName.endsWith(".java") && !fileName.endsWith(".mj")) failure("Files must end with *.java or *.mj!")
      else if(!Files.exists(file)) failure(s"File $file does not exist!")
      else success
    } text ("Input files (*.java or *.mj)")
  }

  parser.parse(args, Config()) map { config =>
    new Compiler(config)
  }
}

case class Config(stopAfter: String = "", files: Seq[Path] = Seq())
