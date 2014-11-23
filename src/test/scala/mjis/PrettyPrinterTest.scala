package mjis

import org.scalatest._
import CompilerTestMatchers._
import util.PrettyPrinter

class PrettyPrinterTest extends FlatSpec with Matchers with Inspectors {

  "The pretty-printer" should "not collapse empty classes" in {
    """class A {}""" should succeedPrettyPrintingWith(
        """class A {
           |}
           |""".stripMargin)
  }

  it should "sort members correctly" in {
    """class A {
       |  public void c() {}
       |  public int a;
       |  public void b() {}
       |  public int b;
       |}""".stripMargin should succeedPrettyPrintingWith(
    """class A {
       |	public void b() { }
       |	public void c() { }
       |	public int a;
       |	public int b;
       |}
       |""".stripMargin)
  }

  it should "collapse else after a block" in {
      """class A {
         |  public void a() {
         |    if (true) {
         |      return;
         |    }
         |    else {}
         |  }
         |}""".stripMargin should succeedPrettyPrintingWith(
      """class A {
         |	public void a() {
         |		if (true) {
         |			return;
         |		} else { }
         |	}
         |}
         |""".stripMargin)
  }

  it should "omit an empty else" in {
      """class HelloWorld
         |{
         |  public int bar(int a, int b) {
         |    if (true) {
         |      int i;
         |    } else ;
         |  }
         |}""".stripMargin should succeedPrettyPrintingWith(
      """class HelloWorld {
         |	public int bar(int a, int b) {
         |		if (true) {
         |			int i;
         |		}
         |	}
         |}
         |""".stripMargin)
  }

  it should "only print empty statements when necessary" in {
      """class HelloWorld
         |{
         |  public int bar(int a, int b) {
         |    if (true)
         |      ;
         |    ;;;;;
         |  }
         |}""".stripMargin should succeedPrettyPrintingWith(
      """class HelloWorld {
         |	public int bar(int a, int b) {
         |		if (true)
         |			;
         |	}
         |}
         |""".stripMargin)
  }

  it should "properly prettyprint the example from the sheet" in {
      """class HelloWorld
         |{
         |  public int c;
         |  public boolean[] array;
         |  public static /* blabla */ void main(String[] args)
         |  { System.out.println( (43110 + 0) );
         |    boolean b = true && (!false);
         |    if (23+19 == (42+0)*1)
         |      b = (0 < 1);
         |    else if (!array[2+2]) {
         |      int x = 0;;
         |      x = x+1;
         |    } else {
         |      new HelloWorld().bar(42+0*1, -1);
         |    }
         |  }
         |  public int bar(int a, int b) { return c = (a+b); }
         |}""".stripMargin should succeedPrettyPrintingWith(
      """class HelloWorld {
         |	public int bar(int a, int b) {
         |		return c = (a + b);
         |	}
         |	public static void main(String[] args) {
         |		(System.out).println(43110 + 0);
         |		boolean b = true && (!false);
         |		if ((23 + 19) == ((42 + 0) * 1))
         |			b = (0 < 1);
         |		else if (!(array[2 + 2])) {
         |			int x = 0;
         |			x = (x + 1);
         |		} else {
         |			(new HelloWorld()).bar(42 + (0 * 1), -1);
         |		}
         |	}
         |	public boolean[] array;
         |	public int c;
         |}
         |""".stripMargin)
  }

  it should "pretty-print many fields, main methods and methods" in {
    """class C {
      |public int x;
      |public static void main(String[] args) {}
      |public int z(int j, A b) {}
      |public X[][] foo(Y[][][] y) {}
      |}""".stripMargin should succeedPrettyPrintingWith(
    """class C {
      |	public X[][] foo(Y[][][] y) { }
      |	public static void main(String[] args) { }
      |	public int z(int j, A b) { }
      |	public int x;
      |}
      |""".stripMargin)
  }

  it should "pretty-print While statements" in {
    "class C { public void test() { while(((1 + 2 + 3))) { ; } } }" should succeedPrettyPrintingWith(
      """class C {
        |  public void test() {
        |    while ((1 + 2) + 3) { }
        |  }
        |}
        |""".stripMargin.replace("  ", "\t"))
  }

  it should "pretty-print if-else statements" in {
    "class C { public void test() { if(true) { } else { int i; } } }" should succeedPrettyPrintingWith(
      """class C {
        |  public void test() {
        |    if (true) { } else {
        |      int i;
        |    }
        |  }
        |}
        |""".stripMargin.replace("  ", "\t"))
    "class C { public void test() { if(true); else { int i; } } }" should succeedPrettyPrintingWith(
      """class C {
        |  public void test() {
        |    if (true)
        |      ;
        |    else {
        |      int i;
        |    }
        |  }
        |}
        |""".stripMargin.replace("  ", "\t"))
    "class C { public void test() { if(true) { } else { test(); } } }" should succeedPrettyPrintingWith(
      """class C {
        |  public void test() {
        |    if (true) { } else {
        |      test();
        |    }
        |  }
        |}
        |""".stripMargin.replace("  ", "\t"))
    "class C { public void test() { if(true) { } else test(); } }" should succeedPrettyPrintingWith(
      """class C {
        |  public void test() {
        |    if (true) { } else test();
        |  }
        |}
        |""".stripMargin.replace("  ", "\t"))
  }

  it should "pretty-print NewArrayExpressions" in {
    "class C { public void test() { new int[42+(3||4)][][][]; } }" should succeedPrettyPrintingWith(
      """class C {
        |  public void test() {
        |    new int[42 + (3 || 4)][][][];
        |  }
        |}
        |""".stripMargin.replace("  ", "\t"))
  }
}
