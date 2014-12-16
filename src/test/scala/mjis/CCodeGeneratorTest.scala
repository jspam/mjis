package mjis

import org.scalatest._
import mjis.CompilerTestMatchers._

class CCodeGeneratorTest extends FlatSpec with Matchers {

  "The C code generator" should "generate code for an example program" in {
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
      |    return;
      |  }
      |}
    """.stripMargin

    val expected = """#include <stdio.h>
      |#include <stdint.h>
      |#include <stdlib.h>
      |
      |struct Test;
      |
      |typedef struct Test {
      |  uint8_t field;
      |  int32_t stuff;
      |  struct Test* ptr;
      |  struct Test** ptrArr;
      |  int32_t** arr;
      |} Test;
      |
      |int main() {
      |  Test$foo(((struct Test*)calloc(1, sizeof(struct Test))), 42, ((int32_t**)calloc(42, sizeof(int32_t*))));
      |}
      |
      |void Test$foo(struct Test* this, int32_t bar, int32_t** baz) {
      |  printf("%d\n", (bar * baz[3][4]));
      |  if (!((this)->field)) {
      |    printf("%d\n", 42);
      |  } else ((this)->field = 1);
      |  while ((this)->field) {
      |    ((this)->field = !((this)->field));
      |  }
      |  (3 + 4);
      |  Test$foo(this, (this)->stuff, baz);
      |  return ;
      |}""".replace("  ", "\t").stripMargin

    input should succeedGeneratingCCodeWith(expected)
  }

}
