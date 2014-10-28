package mjis

class Compiler(config: Config) {
  val phases: List[Phase[_]] = List() // Lexer, Parser, Namer, Typer, etc.

}
