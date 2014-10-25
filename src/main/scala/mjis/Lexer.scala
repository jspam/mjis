package mjis

import java.io.{ByteArrayInputStream, InputStream}

class LexerResult(successCallback: () => Boolean) extends AnalysisResult[Stream[Token]](successCallback) {
  override val result = Stream()
}

class Lexer extends Phase {

  def process(input: String): LexerResult = {
    process(new ByteArrayInputStream(input getBytes "UTF-8"))
  }

  def process(input: InputStream): LexerResult = {
    new LexerResult(() => true)
  }

}
