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
    val sjsIR = JDTCompiler(jdtIR)

    assert(sjsIR.length == 1)
    assert(ASTUtils.astToString(sjsIR.head).contains(expectedIR))
  }

  describe("For simple statements in methods,") {
    it("compiles primitive declarations") {
      val aLong: Long = Int.MaxValue.toLong * 100
      assertStatements("boolean b = true;", "var b: boolean = true")
      assertStatements("byte b = 99;", "var b: byte = ((byte)99)")
      assertStatements("char c = 'x';", "var c: char = 'x'")
      assertStatements("double d = 88.8;", "var d: double = 88.8d")
      assertStatements("float f = 9.9;", "var f: float = ((float)9.9d)")
      assertStatements("float f = 9.9f;", "var f: float = 9.9f")
      assertStatements("int i = 10;", "var i: int = 10")
      assertStatements("long l = 11;", "var l: long = ((long)11)")
      assertStatements(s"long l = ${aLong}L;", "var l: long = 214748364700L")
      assertStatements("short s = 12;", "var s: short = ((short)12)")
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
//    it("instance fields are declared") {
//      assertJava(
//        """
//          |class Test {
//          |    int i = 0;
//          |    float f = 1, g = 2;
//          |}
//          |""".stripMargin,
//        """class Test extends java.lang.Object {
//          |  var i: int
//          |  var f: float
//          |  var g: float
//          |  constructor def <init>;V() {
//          |    this.java.lang.Object::<init>;V();
//          |    this.Test::i = 0;
//          |    this.Test::f = 1.0f;
//          |    this.Test::g = 2.0f
//          |  }
//          |}
//          |""".stripMargin)
//    }

//    it("instance methods are declared") {
//      assertJava(
//        """class Test {
//          |    void mVoid() { return; }
//          |    int mInt(int i) { return i; }
//          |}
//          |""".stripMargin,
//        """class Test extends java.lang.Object {
//          |  def mVoid;V() {
//          |    (void 0)
//          |  }
//          |  def mInt;I;I(var i: int): int = {
//          |    i
//          |  }
//          |  constructor def <init>;V() {
//          |    this.java.lang.Object::<init>;V()
//          |  }
//          |}
//          |""".stripMargin
//      )
//    }
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
      val ast = ASTUtils.javaToSJS(
        """class Test {
          |    static int si = 2;
          |    static int stat(int i) { return si; }
          |    int inst(int i) { return stat(i); }
          |}
          |""".stripMargin)
//      ast.map(_.show).foreach(println)

      val scala = ASTUtils.compileString(
        """object Test {
          |  def foo(): Unit = {}
          |}
          |class Test {
          |  Test.foo()
          |  System.out.println("foo")
          |}
          |""".stripMargin
      )

      val expectedModule =
        """module class Test$ extends java.lang.Object {
          |  var si: int
          |  def stat;I;I(var i: int): int = {
          |    this.Test$::si
          |  }
          |  constructor def <init>;V() {
          |    this.java.lang.Object::<init>;V();
          |    mod:Test$<-this;
          |    this.Test$::si = 2
          |  }
          |}""".stripMargin
      val expectedClass =
        """class Test extends java.lang.Object {
          |  def inst;I;I(var i: int): int = {
          |    mod:Test$.stat;I;I(i)
          |  }
          |  constructor def <init>;V() {
          |    this.java.lang.Object::<init>;V()
          |  }
          |  static def stat;I;I(var i: int): int = {
          |    mod:Test$.stat;I;I(i)
          |  }
          |  static def si;I(): int = {
          |    mod:Test$.si;I()
          |  }
          |  static def si_$eq;I;V{si_=}(x$1: int) {
          |    mod:Test$.si_$eq;I;V(x$1)
          |  }
          |}""".stripMargin

//      assertIREquals(ast(0), expectedModule, "module class")
//      assertIREquals(ast(1), expectedClass, "instance class")
    }

    it("methods should be dispatched properly") {
//      val ast = ASTUtils.javaToSJS(
//        """class Test
//          |    static void staticMethod() {
//          |        staticMethod();
//          |    }
//          |    void instanceMethod() {
//          |        staticMethod();
//          |        instanceMethod();
//          |    }
//          |""".stripMargin)
//      val scala = ASTUtils.compileString(
//        """object Test {
//          |  def staticMethod(): Unit = {}
//          |  def staticMethod2(): Unit = {
//          |    staticMethod()
//          |  }
//          |}
//          |class Test {
//          |  def instanceMethod(): Unit = {
//          |    Test.staticMethod()
//          |    this.instanceMethod()
//          |  }
//          |}
//          |""".stripMargin)
//      scala.map(_.show).foreach(println)
    }
  }

  describe("Playground") {
    it("playground") {
      val ast = ASTUtils.javaToSJS(
        """
          |class Test {
          |  static int si = 3;
          |  static int foo() { return si; }
          |}
          |""".stripMargin)
//      println(ast.head.show)
    }
  }
}
