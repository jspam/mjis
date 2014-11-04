package mjis

import java.io._
import java.nio.file.{Path}
import scala.collection.JavaConversions._

object Compiler {
  def compile(config: Config): Boolean = {
    // Concatenate input files
    val concatenatedInputStream = new SequenceInputStream((config.files map {
      path: Path => new BufferedInputStream(new FileInputStream(path.toFile))
    }).iterator)

    val lexer = new Lexer(new InputStreamReader(concatenatedInputStream, "ASCII"))
    lexer.findings.map { f => System.err.println(f) }
    if (config.stopAfter == "lexer") {
      System.out.println(lexer.dumpResult)
      return lexer.success
    }

    val parser = new Parser(lexer.result)
    parser.findings.map { f => System.err.println(f) }

    parser.success
  }
}
