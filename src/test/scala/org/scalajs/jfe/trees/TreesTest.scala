package org.scalajs.jfe.trees

import org.scalajs.jfe.{ASTUtils, TextUtils => tu}
import org.scalajs.ir.{Names => jsn, Position, Trees => js, Types => jst}
import org.scalatest.BeforeAndAfter
import org.scalatest.funspec.AnyFunSpec

import scala.io.{Codec, Source}

class TreesTest extends AnyFunSpec with BeforeAndAfter {
  before {
    tu.clearFreshNames()
  }

  import org.scalajs.jfe.TestUtils._

  case class CodeTest(name: String, scala: String, java: String)

  val TESTS: Seq[CodeTest] = Seq(
    //    CodeTest("Main method", "MainTest.scala", "MainTest.java")
    //    CodeTest("Features", "FeatureClass.scala", "FeatureClass.java")
  )

  def assertJavaBody(bodySrc: String, expectedIR: String): Unit = {
    val jdtIR = ASTUtils.compileJavaString(
      s"""
         |package test
         |class TestClass {
         |  public void test() {
         |    ${bodySrc}
         |  }
         |}
         |""".stripMargin)
    val sjsIR = JDTCompiler.gen(jdtIR)

    assert(sjsIR.length == 1)
    assert(ASTUtils.astToString(sjsIR.head).contains(expectedIR))
  }

  describe("For simple statements in methods,") {
    it("compiles primitive declarations") {
      assertStatements("boolean b = true;", "var b: boolean = true")
      assertStatements("byte b = 99;", "var b: byte = 99_b")
      assertStatements("char c = 'x';", "var c: char = 'x'")
      assertStatements("double d = 88.8;", "var d: double = 88.8d")
      assertStatements("float f = 9.9;", "var f: float = 9.9f")
      assertStatements("float f = 9.9f;", "var f: float = 9.9f")
      assertStatements("int i = 10;", "var i: int = 10")
      assertStatements("long l = 11;", "var l: long = 11L")
      assertStatements("short s = 12;", "var s: short = 12_s")
    }

    it("declares Objects as the <any> type") {
      assertStatements("Object o = null;", "var o: any = null")
    }

    it("declares null references as the null type") {
      assertStatements("Object o = null;", "var o: any = null")
    }

    it("declares simple reference types") {
      assertStatements("String s = \"asdf\";", "var s: java.lang.String = \"asdf\"")
    }

    it("declares primitive arrays") {
      assertStatements("int[] a = new int[5];", "var a: int[] = new int[5]")
      assertStatements("byte[] a = new byte[6];", "var a: byte[] = new byte[6]")
      assertStatements("float[] a = new float[7];", "var a: float[] = new float[7]")
    }

    it("declares primitive arrays with initializers") {
      assertStatements("int[] a = new int[] { 1, 2, 3 };", "var a: int[] = int[](1, 2, 3)")
      assertStatements("float[] a = new float[] { 1.1f, 2.2f, 3.3f };", "var a: float[] = float[](1.1f, 2.2f, 3.3f)")
      assertStatements("double[] a = new double[] { 1.1, 2.2, 3.3 };", "var a: double[] = double[](1.1d, 2.2d, 3.3d)")
      //      assertSame("byte[] a = new byte[] { 1, 2, 3 };", "val a: Array[Byte] = Array(1, 2, 3)")
    }
  }

  describe("For class declarations,") {
    it("instance fields are declared") {
      assertJava(
        """
          |class Test {
          |    int i = 0;
          |    float f = 1, g = 2;
          |}
          |""".stripMargin,
        """class Test extends java.lang.Object {
          |  var i: int
          |  var f: float
          |  var g: float
          |  constructor def <init>;V() {
          |    this.java.lang.Object::<init>;V();
          |    this.Test::i = 0;
          |    this.Test::f = 1.0f;
          |    this.Test::g = 2.0f
          |  }
          |}
          |""".stripMargin)
    }

    it("instance methods are declared") {
      assertJava(
        """class Test {
          |    void mVoid() { return; }
          |    int mInt(int i) { return i; }
          |}
          |""".stripMargin,
        """class Test extends java.lang.Object {
          |  def mVoid;V() {
          |    _return_0: {
          |      return@_return_0 (void 0)
          |    }
          |  }
          |  def mInt;I;I(var i: int): int = {
          |    _return_1[int]: {
          |      return@_return_1 i
          |    }
          |  }
          |  constructor def <init>;V() {
          |    this.java.lang.Object::<init>;V()
          |  }
          |}
          |""".stripMargin
      )
    }
  }

  describe("For expressions,") {
    it("++ should increment integer types") {
      assertStatements(
        """
          |int i = 1;
          |i++;
          |""".stripMargin,
        """{
          |  var i: int = 1;
          |  i = (i +[int] 1)
          |}""".stripMargin)
    }
  }

  describe("Static") {
    it("members should be declared in a module class") {
      val s = ASTUtils.compileString(
        """
          |object Test {
          |  var si: Int = 2
          |  def stat(i: Int) = si
          |}
          |class Test {
          |  def inst(i: Int) = Test.stat(i)
          |}
          |""".stripMargin)
      s.map(_.show).foreach(println)
      val ast = ASTUtils.javaToSJS(
        """
          |class Test {
          |    static int si = 2;
          |    static int stat(int i) { return si; }
          |    int inst(int i) { return stat(i); }
          |}
          |""".stripMargin)
      println("=== ACTUAL ===")
      ast.map(_.show).foreach(println)

      assertIREquals(ast(1), "")
    }
  }

  describe("IRs match for") {
    TESTS.foreach { test =>
      it(test.name) {
        val scalaSrc = Source.fromResource(test.scala)(Codec.UTF8)
        val scalaIR = ASTUtils.compileString(scalaSrc)
        ASTUtils.printAST(scalaIR.head)
        val javaSrc = Source.fromResource(test.java)(Codec.UTF8)
        val javaIR = JDTCompiler.gen(ASTUtils.compileJavaString(javaSrc))

        //        assert(scalaIR.size == javaIR.size)
        scalaIR.zip(javaIR).foreach { case (scalaCls, javaCls) =>
          assert(ASTUtils.astToString(javaCls) == ASTUtils.astToString(scalaCls))
        }
      }
    }

    //    describe("expression: ") {
    //      def wrapScalaStatements(statements: String*): String =
    //        s"""
    //          |package test
    //          |class TestClass {
    //          |  def test(): Unit = {
    //          |    ${statements.mkString("\n")}
    //          |  }
    //          |}
    //          |""".stripMargin
    //
    //      def wrapJavaStatements(statements: String*): String =
    //        s"""
    //           |package test
    //           |class TestClass {
    //           |  public void test() {
    //           |    ${statements.mkString("\n")}
    //           |  }
    //           |}
    //           |""".stripMargin
    //
    //      it("Array indexing") {
    //        val scalaSrc = wrapScalaStatements(
    //          "val arr = Array(1, 2, 3)",
    //          "arr(1)"
    //        )
    //        val javaSrc = wrapJavaStatements(
    //          "int[] arr = new int[] { 1, 2, 3 };",
    //          "int i = arr[1];"
    //        )
    //        val scalaIR = ASTUtils.compileString(scalaSrc)
    //        val jdtIR = ASTUtils.compileJavaString(javaSrc)
    //        println(jdtIR)
    //        val javaIR = new JavaToSJS(jdtIR).transform()
    //        assert(ASTUtils.astToString(javaIR.head) == ASTUtils.astToString(scalaIR.head))
    //      }
    //    }
  }

  describe("Playground") {
    //    it("continue") {
    //      val ast = ASTUtils.compileString(
    //        """
    //          |class Test {
    //          |
    //          |}
    //          |""".stripMargin)
    //      assert(ast.head.show == "")
    //    }
  }
}
