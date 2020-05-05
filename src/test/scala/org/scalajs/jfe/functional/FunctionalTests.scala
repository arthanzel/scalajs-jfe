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

  describe("Type features:") {
    it("boxes assignments") {
      val src =
        """class Main {
          |    public static void main() {
          |        Boolean bool = true;
          |        Byte b = 10;
          |        Short s = 11;
          |        Integer i = 12;
          |        Long l = 13;
          |        System.out.println(bool);
          |        System.out.println(b);
          |        System.out.println(s);
          |
          |        byte b2 = b;
          |        System.out.println(b2);
          |    }
          |}""".stripMargin
      assertRun(src, Seq(true, 10, 11, 10))
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

  describe("Instances:") {
    it("constructs JDK objects") {
      val src =
        """import java.util.Random;
          |class Main {
          |    public static void main() {
          |        String x = new String("A string");
          |        System.out.println(x);
          |        Random r = new Random(1234);
          |        System.out.println(r.nextInt(10));
          |        Object o = new Object();
          |        System.out.println(o.toString());
          |        //Integer i = new Integer(10);
          |        //System.out.println(new Integer(i.hashCode()));
          |    }
          |}""".stripMargin
      assertRun(src, Seq("A string", 8, "java.lang.Object@1"))
    }

    it("constructs objects") {
      val src =
        """class Main {
          |    public static void main() {
          |        String x = new String("A string");
          |        System.out.println(x);
          |    }
          |}""".stripMargin
      assertRun(src, "A string")
    }

    it("declares instance fields") {
      val src =
        """class Main {
          |    public int i = 10;
          |    public String s = "Instance string";
          |    public static void main() {
          |        Main m = new Main();
          |        System.out.println(m.i);
          |        System.out.println(m.s);
          |    }
          |}
          |""".stripMargin
      assertRun(src, Seq(10, "Instance string"))
    }

    it("sets instance fields") {
      val src =
        """class Main {
          |    public int i = 10;
          |    public String s = "Instance string";
          |    public static void main() {
          |        Main m = new Main();
          |        System.out.println(m.i);
          |        System.out.println(m.s);
          |        m.i = 20;
          |        m.s = "Changed string";
          |        System.out.println(m.i);
          |        System.out.println(m.s);
          |    }
          |}
          |""".stripMargin
      assertRun(src, Seq(10, "Instance string", 20, "Changed string"))
    }

    it("calls instance methods") {
      val src =
        """class Main {
          |    public int inst() {
          |        System.out.println("inst");
          |        return 10;
          |    }
          |
          |    public static void main() {
          |        Main m = new Main();
          |        System.out.println("main");
          |        System.out.println(m.inst());
          |    }
          |}
          |""".stripMargin
      assertRun(src, Seq("main", "inst", 10))
    }

    it("calls different constructors") {
      val src =
        """class Main {
          |    public static void main() {
          |        String s1 = new String("from literal");
          |        String s2 = new String(s1);
          |        String s3 = new String(new byte[] { 98, 121, 116, 101, 115 });
          |        String s4 = new String(new char[] { 'c', 'h', 'a', 'r', 's' });
          |        System.out.println(s1);
          |        System.out.println(s2);
          |        System.out.println(s3);
          |        System.out.println(s4);
          |    }
          |}
          |""".stripMargin
      assertRun(src, Seq("from literal", "from literal", "bytes", "chars"))
    }

    it("calls a custom top-level constructor") {
      val src =
        """
          |class Main {
          |    int field = 10;
          |    int field2 = 20;
          |    public Main(int i) {
          |        this.field2 = i;
          |        System.out.println("zero");
          |    }
          |    public static void main() {
          |       Main a = new Main(30);
          |       System.out.println(a.field);
          |       System.out.println(a.field2);
          |    }
          |}""".stripMargin
      assertRun(src, Seq("zero", 10, 30))
    }

    it("calls a custom top-level constructor calling implicit super()") {
      val src =
        """
          |class Main {
          |    int field = 10;
          |    int field2 = 20;
          |    public Main(int i) {
          |        super();
          |        this.field2 = i;
          |        System.out.println("one");
          |    }
          |    public static void main() {
          |       Main a = new Main(30);
          |       System.out.println(a.field);
          |       System.out.println(a.field2);
          |    }
          |}""".stripMargin
      assertRun(src, Seq("one", 10, 30))
    }

    it("calls a custom top-level constructor calling co-constructor") {
      val src =
        """
          |class Main {
          |    int field = 10;
          |    int field2 = 20;
          |    int field3 = 30;
          |    public Main(int i, int j) {
          |        this(i);
          |        this.field3 = j;
          |    }
          |    public Main(int i) {
          |        this.field2 = i;
          |        System.out.println("zero");
          |    }
          |    public static void main() {
          |       Main a = new Main(30, 40);
          |       System.out.println(a.field);
          |       System.out.println(a.field2);
          |       System.out.println(a.field3);
          |    }
          |}""".stripMargin
      assertRun(src, Seq("zero", 10, 30, 40))
    }

    it("accesses members from other custom classes") {
      val src =
        """class Other {
          |    public int number = 3;
          |    public int method(int x) {
          |         System.out.println("Other#method");
          |         System.out.println(x);
          |         return 10;
          |     }
          |}
          |class Main {
          |    public static void main() {
          |        Other other = new Other();
          |        System.out.println(other.number);
          |        other.number = 4;
          |        System.out.println(other.number);
          |        System.out.println(other.method(5));
          |    }
          |}
          |
          |""".stripMargin
      assertRun(src, Seq(3, 4, "Other#method", 5, 10))
    }

    it("inherits stuff") {
      val src =
        """class Base {
          |    public String cat = "cat";
          |    public String dog = "dog";
          |    public String say() { return "meow"; }
          |}
          |class Main extends Base {
          |    public String dog = "poodle";
          |    public String say() { return "bark"; }
          |    public static void main() {
          |        Base b = new Base();
          |        System.out.println(b.cat);
          |        System.out.println(b.dog);
          |        System.out.println(b.say());
          |        Main m = new Main();
          |        System.out.println(m.cat);
          |        System.out.println(m.dog);
          |        System.out.println(m.say());
          |        Base mb = new Main();
          |        System.out.println(mb.cat);
          |        System.out.println(mb.dog);
          |        System.out.println(mb.say());
          |    }
          |}""".stripMargin
      assertRun(src, Seq(
        "cat", "dog", "meow",
        "cat", "poodle", "bark",
        "cat", "dog", "bark"
      ))
    }

    it("calls super fields and methods") {
      val src =
        """class A {
          |    public String field = "fieldA";
          |    public String method() { return "methodA"; }
          |}
          |class B extends A {
          |    public String field = "fieldB";
          |    public String method() { return "methodB"; }
          |    public String superField() { return super.field; }
          |    public String superMethod() { return super.method(); }
          |}
          |class Main {
          |    public static void main() {
          |        B b = new B();
          |        System.out.println(b.field);
          |        System.out.println(b.method());
          |        System.out.println(b.superField());
          |        System.out.println(b.superMethod());
          |    }
          |}
          |""".stripMargin
      val scala =
        """
          |object O {
          |  new C().method()
          |}
          |class C {
          |  def method() = super.hashCode()
          |}
          |""".stripMargin
      assertRun(src, Seq("fieldB", "methodB", "fieldA", "methodA"))
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
