package mjis

import mjis.ast._
import org.scalatest._
import CompilerTestHelper._
import CompilerTestMatchers._
import java.nio.file.Paths

class ParserTest extends FlatSpec with Matchers with Inspectors {

  implicit val pos: Position = Position.NoPosition

  def repeat(str: String, count: Integer) = Seq.fill(count)(str).mkString("")

  def statementsAST(innerAST: Statement*) = Program(List(
    ClassDecl("Test", List(
      MethodDecl("test", List(Parameter("this", TypeBasic("Test"), isWritable = false)), Builtins.VoidType,
        Block(innerAST.toList)
      ),
      MethodDecl("main",
        List(Parameter("args", TypeArray(TypeBasic("String"), 1), isReadable = false, isWritable = false)),
        Builtins.VoidType, Block(List()), isStatic = true
      )
    ), List())
  ))
  def expressionsAST(innerAST: Expression*) = statementsAST(innerAST.map(exprStmt => ExpressionStatement(exprStmt)): _*)

  /* class declarations */

  "The parser" should "accept an empty program" in {
    "" should succeedParsingWith(Program(Nil))
  }

  it should "accept a program consisting of an empty class" in {
    "class Test { }" should succeedParsingWith(Program(List(ClassDecl("Test", Nil, Nil))))
  }

  it should "accept many empty classes" in {
    repeat("class A { }", 10000) should succeedParsing()
  }

  it should "accept fields of any type" in {
    "class C { public int x; public boolean y; public void z; public MyType u; public MyType[][] v;}" should
      succeedParsingWith(
        Program(List(
          ClassDecl("C", Nil, List(
            FieldDecl("x", Builtins.IntType),
            FieldDecl("y", Builtins.BooleanType),
            FieldDecl("z", Builtins.VoidType),
            FieldDecl("u", TypeBasic("MyType")),
            FieldDecl("v", TypeArray(TypeBasic("MyType"), 2)))))))
  }

  it should "accept many fields, main methods and methods" in {
    "class C {" + repeat("""|public int x;
                    |public static void main(String[] args) {}
                    |public int z(int j, A b) {}""".stripMargin,
                    10000) + "}" should succeedParsing()
  }

  it should "accept main methods with any name" in {
    "class C { public static void foobar(String[] args) {} }" should succeedParsingWith(Program(List(
      ClassDecl("C", List(
        MethodDecl("foobar",
          List(Parameter("args", TypeArray(TypeBasic("String")), isReadable = false, isWritable = false)),
          Builtins.VoidType, Block(List()), isStatic=true)
      ), List())
    )))
  }

  /* statements */

  it should "accept an empty block" in {
    fromStatements("") should succeedParsingWith(statementsAST())
  }

  it should "accept a nested empty block" in {
    fromStatements("{}") should succeedParsingWith(statementsAST(Block(List())))
  }

  it should "accept a program with different local variable declarations" in {
    fromStatements("int a; boolean b; myType[] c = xyz; myType x = 42;") should succeedParsingWith(statementsAST(
      LocalVarDeclStatement("a", Builtins.IntType, None),
      LocalVarDeclStatement("b", Builtins.BooleanType, None),
      LocalVarDeclStatement("c", TypeArray(TypeBasic("myType")), Some(Ident("xyz"))),
      LocalVarDeclStatement("x", TypeBasic("myType"), Some(IntLiteral("42")))
    ))
  }

  it should "accept many nested blocks" in {
    // {{{{...}}{{...}}}}
    fromStatements(repeat("{", 10000) + repeat("}", 5000)
      + repeat("{", 5000) + repeat("}", 10000)) should succeedParsing()
  }

  it should "accept many nested if-else constructs" in {
    // if(1){if(1){...}}else{if(1){...}}
    fromStatements(repeat("if(1){", 10000) + repeat("}", 10000)
      + "else " + repeat("if(1){", 10000) + repeat("}", 10000)) should succeedParsing()
  }

  it should "accept many nested and consecutive while loops" in {
    fromStatements(repeat("while(0) {", 10000) + repeat("}", 10000)
        + repeat("while(0);", 10000)) should succeedParsing()
  }

  it should "accept a program with many return statements" in {
    fromStatements(repeat("return 0;", 10000)) should succeedParsing()
  }

  it should "properly recognize expression statements" in {
    // this is interesting because it's a spot where the grammar isn't SLL(1)
    "class a { public void foo ( ) { a [ 2 ] ; } }" should succeedParsingWith(Program(List(
      ClassDecl("a", List(
        MethodDecl("foo", List(Parameter("this", TypeBasic("a"), isWritable = false)), Builtins.VoidType, Block(List(
          ExpressionStatement(Apply("[]", List(Ident("a"), IntLiteral("2")), isOperator=true))
        )))
      ), List())
    )))
  }

  /* expressions */

  it should "accept primary expressions" in {
    fromStatements(
      """
        |null;
        |false;
        |true;
        |1337;
        |myVar;
        |myFunc();
        |this;
        |(null);
        |new myType();
        |new myType[3+x][][];
        |new int[3+x][][];
      """.stripMargin) should succeedParsingWith(expressionsAST(
      NullLiteral(),
      FalseLiteral(),
      TrueLiteral(),
      IntLiteral("1337"),
      Ident("myVar"),
      Apply("myFunc", List(ThisLiteral())),
      ThisLiteral(),
      NullLiteral(),
      NewObject(TypeBasic("myType")),
      NewArray(TypeBasic("myType"), Apply("+", List(IntLiteral("3"), Ident("x")), isOperator=true), 2),
      NewArray(Builtins.IntType, Apply("+", List(IntLiteral("3"), Ident("x")), isOperator=true), 2)
    ))
  }

  it should "accept and transform MinInt" in {
    fromStatements("-2147483648;") should succeedParsingWith(expressionsAST(IntLiteral("-2147483648")))
  }

  it should "accept assignments" in {
    fromStatements("a=a;\na=a=a;") should succeedParsingWith(expressionsAST(
      Assignment(Ident("a"), Ident("a")),
      Assignment(Ident("a"), Assignment(Ident("a"), Ident("a")))
    ))
  }

  it should "accept long assignment chains" in {
    // a=a=a=...=a;
    fromStatements(repeat("a=", 10000) + "a;") should succeedParsing()
  }

  it should "accept method calls with parameters" in {
    fromStatements("a(b);\na(b, c);\na(b, c(d));") should succeedParsingWith(expressionsAST(
      Apply("a", List(ThisLiteral(), Ident("b"))),
      Apply("a", List(ThisLiteral(), Ident("b"), Ident("c"))),
      Apply("a", List(ThisLiteral(), Ident("b"), Apply("c", List(ThisLiteral(), Ident("d")))))
    ))
  }

  it should "accept long method call chains" in {
    // a(a(...a(null)...));
    fromStatements(repeat("a(", 10000) + "null" + repeat(")", 10000) + ";") should succeedParsing()
  }

  it should "accept long parenthesis chains" in {
    // ((((...((a))...))));
    fromStatements(repeat("(", 10000) + "a" + repeat(")", 10000) + ";") should succeedParsing()
  }

  it should "accept long chains of alternating expressions and parentheses" in {
    // a+(a+(a+(a+(...a+(a+(a))...))));
    fromStatements(repeat("a+(", 10000) + "a" + repeat(")", 10000) + ";") should succeedParsing()
  }

  it should "accept unary expressions" in {
    fromStatements("-c;\n-(-c);\n!c;\n!!c;\n!-!-c;\n!(-(!(-(c))));") should succeedParsingWith(expressionsAST(
      Apply("-", List(Ident("c")), isOperator=true),
      Apply("-", List(Apply("-", List(Ident("c")), true)), true),
      Apply("!", List(Ident("c")), true),
      Apply("!", List(Apply("!", List(Ident("c")), true)), true),
      Apply("!", List(Apply("-", List(Apply("!", List(Apply("-", List(Ident("c")), true)), true)), true)), true),
      Apply("!", List(Apply("-", List(Apply("!", List(Apply("-", List(Ident("c")), true)), true)), true)), true)
    ))
  }

  it should "accept long chains of unary expressions" in {
    // a+(a+(a+(a+(...a+(a+(a))...))));
    fromStatements(repeat("!-", 10000) + "a;") should succeedParsing()
  }

  it should "accept binary expressions" in {
    fromStatements("a+b;a-b;a*b;a/b;a%b;a&&b;a||b;a>b;a<b;a<=b;a>=b;a==b;a!=b;") should succeedParsingWith(expressionsAST(
      List("+", "-", "*", "/", "%", "&&", "||", ">", "<", "<=", ">=", "==", "!=").map(Apply(_, List(Ident("a"), Ident("b")), isOperator=true)): _*
    ))
  }

  it should "accept binary expression chains" in {
    fromStatements("a+b*c+d;") should succeedParsingWith(expressionsAST(
      Apply("+", List(
        Apply("+", List(
          Ident("a"),
          Apply("*", List(
            Ident("b"),
            Ident("c")
          ), isOperator=true)
        ), isOperator=true),
        Ident("d")
      ), isOperator=true)
    ))
  }

  it should "accept nested binary expressions" in {
    fromStatements("a+b*c-d!=(e>f*g);") should succeedParsingWith(expressionsAST(
      Apply("!=", List(
        Apply("-", List(
          Apply("+", List(
            Ident("a"),
            Apply("*", List(
              Ident("b"),
              Ident("c")),
              isOperator=true
            )
          ), true),
          Ident("d")
        ), true),
        Apply(">", List(
          Ident("e"),
          Apply("*", List(
            Ident("f"),
            Ident("g")
          ), true)
        ), true)
      ), true)
    ))
  }

  it should "accept nested expressions containing assignments" in {
    fromStatements("a||b=c||d;") should succeedParsingWith(expressionsAST(
      Assignment(
        Apply("||", List(
          Ident("a"),
          Ident("b")
        ), isOperator=true),
        Apply("||", List(
          Ident("c"),
          Ident("d")
        ), true)
      )
    ))
  }

  it should "accept array creations" in {
    fromStatements("new int[5]; new int[a][][]; new a[c-(d)][];") should succeedParsingWith(expressionsAST(
      NewArray(Builtins.IntType, IntLiteral("5"), 0),
      NewArray(Builtins.IntType, Ident("a"), 2),
      NewArray(TypeBasic("a"), Apply("-", List(Ident("c"), Ident("d")), isOperator=true), 1)
    ))
  }

  it should "accept field accesses and method calls" in {
    fromStatements("a.b.c;a.b.c();a[b].c.c()[d];") should succeedParsingWith(expressionsAST(
      Select(Select(Ident("a"), "b"), "c"),
      Apply("c", List(Select(Ident("a"), "b"))),
      Apply("[]", List(
        Apply("c", List(
          Select(
            Apply("[]", List(
              Ident("a"),
              Ident("b")
            ), isOperator=true),
            "c"
          )
        )),
        Ident("d")
      ), true)
    ))
  }

  it should "accept long chains of field accesses" in {
    fromStatements(repeat("a.", 10000) + "b;") should succeedParsing()
  }

  it should "accept array access into new arrays" in {
    fromStatements("new array[10][][1];") should succeedParsingWith(expressionsAST(
      Apply("[]", List(NewArray(TypeBasic("array"), IntLiteral("10"), 1), IntLiteral("1")), isOperator=true)
    ))
  }

  /* negative test cases */

  it should "reject arbitrary expressions in member accesses" in {
    val findings = assertExecFailure[Parser](fromStatements("a.(b+c);"))
    findings.head shouldBe a [Parser.UnexpectedTokenError]
  }

  it should "reject a class declaration without class name" in {
    val findings = assertExecFailure[Parser]("class { }")
    findings.head shouldBe a [Parser.UnexpectedTokenError]
  }

  it should "reject a program with a premature EOF" in {
    val findings = assertExecFailure[Parser]("class")
    findings.head shouldBe a [Parser.UnexpectedTokenError]
    findings.head.asInstanceOf[Parser.UnexpectedTokenError].token.data shouldBe TokenData.EOF
    findings.head.pos.column shouldBe 6  // the char after "class"
  }

  it should "reject a program that declares variables inside non-block conditional scope" in {
    val findings = assertExecFailure[Parser]("class a { public void foo ( ) { if ( a ) int i ; } }")
    findings.head shouldBe a [Parser.UnexpectedTokenError]
    findings.head.pos.column shouldBe 42  // beginning of int
  }

  it should "reject an attept to create an array of something other than a basic type" in {
    val findings = assertExecFailure[Parser](fromStatements("new 3[4];"))
    findings.head shouldBe a [Parser.UnexpectedTokenError]
  }

  it should "reject invalid main methods" in {
    val tests = List("public static void main(int[] args) {}", "public static void main(String args) {}",
      "public static void main(MyClass args) {}", "public static void main(MyClass2 args) {}")
    all(tests.map(p => "class Test {" + p + "}")) shouldNot succeedParsing()
  }

  it should "reject invalid 'new' expressions" in {
    val tests = List("new 3;", "new a[][6];", "new;", "new class()", "new MyClass;", "new MyClass(3);")
    all(tests.map(fromStatements(_))) shouldNot succeedParsing()
  }

  it should "reject invalid field declarations" in {
    val tests = List("public 3;", "public int;", "public void x(3, 4);", "public void[3] foo;",
      "public int a, b;")
    all(tests.map(p => "class Test{" + p + "}")) shouldNot succeedParsing()
  }
}
