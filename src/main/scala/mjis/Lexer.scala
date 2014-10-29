package mjis

import java.io.{ByteArrayInputStream, InputStream}

import mjis.TokenData.{IntegerLiteral, Identifier}

class Lexer(val input: InputStream) extends AnalysisPhase[Stream[Token]] {

  def this(input: String) = {
    this(new ByteArrayInputStream(input getBytes "UTF-8"))
  }

  override def getResult(): Stream[Token] = {
    Stream[Token]()
  }

  override def successCallback(): Boolean = true

  override def dumpResult(): String = {
    var dump = result.map(token => token.data match {
      case Identifier(literal) => s"identifier $literal"
      case IntegerLiteral(literal) => s"integer literal $literal"
      case _ => token.data.literal
    })

    // TODO: Only append EOF if the file was read completely
    dump :+= "EOF"

    if (!this.success) {
      dump :+= "error"
    }

    dump.mkString(System.lineSeparator()) + System.lineSeparator()
  }
}
