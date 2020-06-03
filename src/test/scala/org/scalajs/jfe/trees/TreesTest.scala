package org.scalajs.jfe.trees

import org.scalajs.jfe.ASTUtils
import org.scalajs.jfe.util.{TextUtils => tu}
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
      s"""package test;
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
}
