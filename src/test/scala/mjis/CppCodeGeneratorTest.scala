package mjis

import org.scalatest._
import mjis.CompilerTestMatchers._

class CppCodeGeneratorTest extends FlatSpec with Matchers {

  "The C++ code generator" should "generate code for an example program" in {
    val input = """
      |class Test {
      |  public boolean field;
      |  public int stuff;
      |  public Test ptr;
      |  public Test[] ptrArr;
      |  public int[][] arr;
      |
      |  public static void main(String[] args) {
      |    new Test().foo(42, new int[42][]);
      |  }
      |
      |  public void foo(int bar, int[][] baz) {
      |    System.out.println(bar * baz[3][4]);
      |    if (!field) {
      |      System.out.println(42);
      |    } else field = true;
      |    while (field) {
      |      field = !field;
      |    }
      |    3 + 4;
      |    this.foo(this.stuff, baz);
      |  }
      |}
    """.stripMargin

    val expected = """#include <cstdio>
      |#include <cstdint>
      |
      |class Test {
      |public:
      |  Test() : field(false), stuff(0), ptr(NULL), ptrArr(NULL), arr(NULL) {}
      |  bool field;
      |  int32_t stuff;
      |  Test* ptr;
      |  Test** ptrArr;
      |  int32_t** arr;
      |  void foo(int32_t bar, int32_t** baz) {
      |    printf("%d\n", (bar * baz[3][4]));
      |    if (!(field)) {
      |      printf("%d\n", 42);
      |    } else (field = true);
      |    while (field) {
      |      (field = !(field));
      |    }
      |    (3 + 4);
      |    (this)->foo((this)->stuff, baz);
      |  }
      |};
      |
      |int main() {
      |  (new Test())->foo(42, new int32_t*[42]);
      |}""".replace("  ", "\t").stripMargin

    input should succeedGeneratingCppCodeWith(expected)
  }

}
