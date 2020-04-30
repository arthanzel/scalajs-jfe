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

  it("links Java programs and prints to the console") {
    val src =
      """class Main {
        |    public static void main() {
        |        System.out.println("The linker works");
        |    }
        |}
        |""".stripMargin
    assertRun(src, "The linker works")
  }

  describe("Javalib:") {
    it("calls JDK static methods") {
      val src =
        """class Main {
          |    public static void main() {
          |        System.out.println(Integer.toHexString(200));
          |    }
          |}""".stripMargin
      assertRun(src, "c8")
    }
  }

  describe("Language features:") {
    describe("Flow control:") {
    it("handles if-else statements") {
      val src =
        """class Main {
          |    public static void main() {
          |        if (true) System.out.println("true 1");
          |        else System.out.println("false 1");
          |        if (false) System.out.println("true 2");
          |        else System.out.println("false 2");
          |    }
          |}""".stripMargin
      assertRun(src, Seq("true 1", "false 2"))
    }

    it("handles if-elseif-else statements") {
      val src =
        """class Main {
          |    public static void main() {
          |        if (false) System.out.println("one");
          |        else if (true) System.out.println("two");
          |        else System.out.println("three");
          |    }
          |}""".stripMargin
      assertRun(src, "two")
    }

      it("handles simple for loops") {

      }
  }

    it("handles literals") {
      val src =
        """class Main {
          |    public static void main() {
          |        System.out.println(10); // int
          |        System.out.println(11L); // long
          |        System.out.println(12.5f); // float
          |        System.out.println(13.5d); // double
          |        System.out.println(true);
          |        System.out.println('@');
          |    }
          |}""".stripMargin
      assertRun(src, Seq(10, 11, 12.5, 13.5, true, '@'))
    }
  }

  describe("Static fields") {
    it("are initialized and can be read") {
      val src =
        """class Main {
          |    static String stat = "A static field";
          |    public static void main() {
          |        System.out.println(stat);
          |    }
          |}""".stripMargin
      assertRun(src, "A static field")
    }

    it("are initialized and can be written") {
      val src =
        """class Main {
          |    static String stat = "A static field";
          |    public static void main() {
          |        System.out.println(stat);
          |        stat = "A changed static field";
          |        System.out.println(stat);
          |    }
          |}""".stripMargin
      assertRun(src, Seq("A static field", "A changed static field"))
    }

    it("can be accessed with fully qualified names") {
      val src =
        """class Main {
          |    static String stat = "A static field";
          |    public static void main() {
          |        System.out.println(Main.stat);
          |        Main.stat = "A changed static field";
          |        System.out.println(Main.stat);
          |    }
          |}""".stripMargin
      assertRun(src, Seq("A static field", "A changed static field"))
    }

    it("support all primitive types") {
      // TODO: This test needs to support implicit coercion of byte->int and short->int
      val src =
        """class Main {
          |    static byte b = 100;
          |    static short s = 101;
          |    static int i = 102;
          |    static long l = 103;
          |    static float f = 104.5;
          |    static double d = 105.5;
          |    static boolean bool = true;
          |    static char c = '@';
          |    public static void main() {
          |        System.out.println((int) b);
          |        System.out.println((int) s);
          |        System.out.println(i);
          |        System.out.println(l);
          |        System.out.println(f);
          |        System.out.println(d);
          |        System.out.println(bool);
          |        System.out.println(c);
          |    }
          |}""".stripMargin
      assertRun(src, Seq(100, 101, 102, 103, 104.5, 105.5, true, '@'))
    }
  }

  describe("Static methods") {
    it("can be defined and called") {
      val src =
        """class Main {
          |    static void one() {
          |        System.out.println("one");
          |    }
          |    static void two() {
          |        System.out.println("two");
          |    }
          |    public static void main() {
          |        System.out.println("main");
          |        one();
          |        two();
          |    }
          |}""".stripMargin
      assertRun(src, Seq("main", "one", "two"))
    }

    it("can be defined an called with fully qualified names") {
      val src =
        """class Main {
          |    static void one() {
          |        System.out.println("one");
          |    }
          |    static void two() {
          |        System.out.println("two");
          |    }
          |    public static void main() {
          |        System.out.println("main");
          |        Main.one();
          |        Main.two();
          |    }
          |}""".stripMargin
      assertRun(src, Seq("main", "one", "two"))
    }

    it("can return values") {
      val src =
        """class Main {
          |    static String makeString() {
          |        return "A string";
          |    }
          |    public static void main() {
          |        System.out.println(makeString());
          |        System.out.println(Main.makeString());
          |    }
          |}""".stripMargin
      assertRun(src, Seq("A string", "A string"))
    }

    it("have side-effects") {
      val src =
        """class Main {
          |    static String stat = "A string"
          |    static void modify() {
          |        stat = "A modified string";
          |    }
          |    public static void main() {
          |        System.out.println(stat);
          |        modify();
          |        System.out.println(stat);
          |    }
          |}""".stripMargin
      assertRun(src, Seq("A string", "A modified string"))
    }
  }

  describe("Classes") {}
}
