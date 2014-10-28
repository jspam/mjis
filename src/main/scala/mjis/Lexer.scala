package mjis

import java.io.{ByteArrayInputStream, InputStream}

class Lexer(val input: InputStream) extends Phase[Stream[Token]] with AnalysisPhase {

  def this(input: String) = {
    this(new ByteArrayInputStream(input getBytes "UTF-8"))
  }

  override def getResult(): Stream[Token] = {
    Stream[Token]()
  }

  override def successCallback(): Boolean = true
}
