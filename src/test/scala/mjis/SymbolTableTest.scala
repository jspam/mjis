package mjis

import mjis.ast.{Decl, TypeBasic, LocalVarDeclStatement}
import org.scalatest._

class SymbolTableTest extends FlatSpec with Matchers with Inspectors {

  implicit val pos: Position = Position.NoPosition

  val foo = LocalVarDeclStatement("foo", TypeBasic("Foo"), None)
  val foo2 = LocalVarDeclStatement("foo", TypeBasic("Foo"), None)
  val bar = LocalVarDeclStatement("bar", TypeBasic("Bar"), None)

  "The symbol table" should "store declarations" in {
    val table = new SymbolTable[Decl]()
    table.enterScope()
    table.insert(foo)

    table.lookup("foo") shouldBe Some(foo)

    table.leaveScope()
  }

  it should "find declarations in outer scopes" in {
    val table = new SymbolTable[Decl]()
    table.enterScope()
    table.insert(foo)
    table.enterScope()

    table.lookup("foo") shouldBe Some(foo)
  }

  it should "forget declarations after leaving their respective scope" in {
    val table = new SymbolTable[Decl]()
    table.enterScope()
    table.insert(foo)
    table.enterScope()
    table.insert(bar)
    table.leaveScope()

    table.lookup("foo") shouldBe Some(foo)
    table.lookup("bar") shouldBe None
  }

  it should "report the current scope correctly" in {
    val table = new SymbolTable[Decl]()
    table.enterScope()
    table.insert(foo)
    table.enterScope()
    table.insert(bar)

    table.inCurrentScope("foo") shouldBe false
    table.inCurrentScope("bar") shouldBe true
  }

  it should "allow same-named declarations in different scopes" in {
    val table = new SymbolTable[Decl]()
    table.enterScope()
    table.insert(foo)
    table.enterScope()
    table.insert(foo2)

    table.inCurrentScope("foo") shouldBe true
    table.lookup("foo").get should be theSameInstanceAs foo2

    table.leaveScope()
    table.inCurrentScope("foo") shouldBe true
    table.lookup("foo").get should be theSameInstanceAs foo
  }
}
