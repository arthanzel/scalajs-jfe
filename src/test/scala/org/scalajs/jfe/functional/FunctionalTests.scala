package org.scalajs.jfe.functional

import org.scalajs.jfe.{ASTUtils, Runner, TextUtils}
import org.scalajs.logging.ScalaConsoleLogger
import org.scalatest.BeforeAndAfter
import org.scalatest.funspec.AnyFunSpec

class FunctionalTests extends AnyFunSpec with BeforeAndAfter {
  import org.scalajs.jfe.TestUtils._

  before {
    TextUtils.clearFreshNames()
  }

  it("links Java programs") {
    val src =
      """class Main {
        |    public static void main() {
        |        System.out.println("The linker works");
        |    }
        |}
        |""".stripMargin
    assertRun(src, "The linker works")
  }

  it("statics") {
    val src =
      """class Main {
        |    public static String stat = "The linker works";
        |    public static void main() {
        |        System.out.println(stat);
        |    }
        |}
        |""".stripMargin
    val classDef = ASTUtils.javaToSJS(src).head
    println(classDef.show)
    Runner.linkAndRun(classDef, "Main")
//    assertRun(src, "The linker works")
  }
}
