package mjis

import java.io.{ByteArrayInputStream, InputStream}

class Lexer {

  def process(input: String): Stream[Token] = {
    process(new ByteArrayInputStream(input getBytes "UTF-8"))
  }

  def process(input: InputStream): Stream[Token] = {
    Stream()
  }

}
